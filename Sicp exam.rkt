#lang racket
(require data-science-master)
(require plot)
(require math)
(require json)
(require srfi/19)
(require racket/stream)


;This function reads line-oriented JSON as output by massmine and packages it into an array. For very large data sets, loading everything into memory like this is heavy handed. For data this small,working in memory is simpler


(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))

;Normalize case, remove URLs,punctuation and spaces from each tweet. This function takes a list of words and returns a preprocessed subset of words as a list

(define (preprocess-text lst)
  (map (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x))) #:websafe? #t))
       lst))

; Using general ugandan tweets

(define tweets (string->jsexpr
                (with-input-from-file "uganda-tweets.json" (λ () (json-lines->json-array)))))

;Extracting each string and appending it one large string using tail recursion.

(define list-string
  (let ([tmp (map (λ (x) (list (hash-ref x 'text)(hash-ref x 'created_at) (hash-ref x 'source))) tweets)]) ;; improve to use streams
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)
    ))

; Joining tweets in a systematic flow.

(define tweets_joined
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 list-string "")) )

;Extracting each word and its number of occurrences.
(define words (document->tokens tweets_joined #:sort? #t))


(define android (filter (λ (x) (string=? (second x) "android")) list-string))
(define iphone (filter (λ (x) (string=? (second x) "iphone")) list-string))

;labeling non stop-word with emotional label using the nrc lexicon.  
(define sentiment (list->sentiment words #:lexicon 'nrc))

(take sentiment 5)

;Aggregating the same sentiment labels.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))


;Visualize the result as a barplot (discrete-histogram)and remove stopwords words
(define clean_texts (remove-punctuation (remove-urls tweets_joined)))
(define word-counts (document->tokens clean_texts #:sort? #t))
(define clean-word-counts (remove-stopwords word-counts))
(define moods (list->sentiment clean-word-counts #:lexicon 'nrc))
(take moods 5)

(aggregate sum ($ moods 'sentiment) ($ moods 'freq))

(let ([counts (aggregate sum ($ moods 'sentiment) ($ moods 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "blue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))

(define moods2 (list->sentiment clean-word-counts #:lexicon 'bing))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ moods2 'sentiment) ($ moods2 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
	 #:color "red"
	 #:line-color "MediumOrchid")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))

(let ([counts (aggregate sum ($ moods2 'sentiment) ($ moods2 'freq))])
  (chi-square-goodness counts '(.5 .5)))

;Looking at words contributing the most to the positive and negative sentiment scores. 
(define negative-tokens
  (take (cdr (subset moods2 'sentiment "negative")) 10))
(define positive-tokens
  (take (cdr (subset moods2 'sentiment "positive")) 10))

;Reshaping for plotting purposes
(define n (map (λ (x) (list (first x) (- 0 (third x))))
	       negative-tokens))
(define p (sort (map (λ (x) (list (first x) (third x)))
		     positive-tokens)
		(λ (x y) (< (second x) (second y)))))
;Plot results
(parameterize ((plot-width 800)
	       (plot-x-tick-label-anchor 'right)
	       (plot-x-tick-label-angle 90))
  (plot (list
	 (tick-grid)
	 (discrete-histogram n #:y-min -120
			     #:y-max 655
			     #:color "blue"
			     #:line-color "white"
			     #:label "Negative Sentiment") 
	 (discrete-histogram p #:y-min -116
			     #:y-max 649
			     #:x-min 15
			     #:color "green"
			     #:line-color "LightSeaGreen"
			     #:label "Positive Sentiment"))
	#:x-label "Word"
	#:y-label "Contribution to sentiment"))
	
;;; Plot top 20 words
(parameterize ([plot-width 600]
               [plot-height 600])
    (plot (list
        (tick-grid)
        (discrete-histogram (reverse (take clean-word-counts 50))
                            #:invert? #t
                            #:color "green"
                            #:line-color "white"
                            #:y-max 450))
       #:x-label "Occurances"
       #:y-label "Words"))

;Analysing time of tweets
(string->date "11/10/2020" "~m/~d/~Y")

(display (string->date "2021 06 05T01:58:13 000Z" "~Y ~m ~dT~H:~M:~S 000Z"))

(define (convert-timestamp str)
  (string->date str "~Y ~m ~dT~H:~M:~S 000Z"))

;Tweets and timestamps 


(define timestamp-by-type
  (map (λ (x) (list (second x) (first x)
                    ))
       list-string))

(define (bin-timestamps timestamps)
  (let ([time-format "~H"])
    ; return
    (sorted-counts
     (map (λ (x) (date->string (convert-timestamp x) time-format)) timestamps))))



(define a-time
  (bin-timestamps ($ (subset timestamp-by-type 1 (λ (x) (string=? x ))) 0)))

(define a-time2 (map (λ (x) (list (string->number (first x)) (second x))) a-time))

(define (fill-missing-bins lst bins)
  (define (get-count lst val)
    (let ([count (filter (λ (x) (equal? (first x) val)) lst)])
      (if (null? count) (list val 0) (first count))))
  (map (λ (bin) (get-count lst bin))
       bins))

;Converting UTC to EST
(define (time-EST lst)
  (map list
       ($ lst 0)
       (append (drop ($ lst 1) 4) (take ($ lst 1) 4))))

;Converting bin counts to percentages
(define (count->percent lst)
  (let ([n (sum ($ lst 1))])
    (map list
         ($ lst 0)
         (map (λ (x) (* 100 (/ x n))) ($ lst 1)))))

(let ([a-data (count->percent (time-EST (fill-missing-bins a-time2 (range 24))))]
     )
  (parameterize ([plot-legend-anchor 'top-right]
                 [plot-width 800])
      (plot (list
             (tick-grid)
             (lines a-data
                    #:color "red"
                    #:width 2
                    #:label "Time of tweeting")
             )
            #:x-label "Hour of day (EST)"
            #:y-label "% of tweets per hour")))

(define (bin-month months)
  (let ([time-format "~m"])
    ; return
    (sorted-counts
     (map (λ (x) (date->string (convert-timestamp x) time-format)) months))))

(define tweet_by_month
  (bin-month ($ (subset timestamp-by-type 1 (λ (x) (string=? x ))) 0)))

(define tweet_by_month2 (map (λ (x) (list (string->number (first x)) (second x))) tweet_by_month))

(let ([month-data (count->percent (fill-missing-bins tweet_by_month2 (range 12)))]
     )
  (parameterize ([plot-legend-anchor 'top-right]
                 [plot-width 800])
      (plot (list
             (tick-grid)
             (lines month-data
                    #:color "red"
                    #:width 2
                    #:label "Teets in a given month")
             )
            #:x-label "Month of the year "
            #:y-label "% of tweets in a given month")))

;Ploting tweets per month
(plot (list (discrete-histogram tweet_by_month
                                #:label "Number of tweets per month"
                                #:skip 2.5
                                #:x-min 0
                                #:color "blue"
                                #:line-color "blue")
            )
      #:y-max 1000
      #:x-label "Month of the year"
      #:y-label "Number of tweets in the month")

(define (bin-month2 months)
  (let ([time-format "~m"])
    ;; Return
   (date->string (convert-timestamp months) time-format)
     ))
(define get-data-by-month
  (map (λ (x) (list (second x)
                    (cond [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "1") "1"]
                          [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "2") "2"]
                           [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "3") "3"]
                            [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "4") "4"]
                            [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "5") "5"]
                           [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "6") "6"]
                            [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "7") "7"]
                            [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "8") "8"]
                            [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "9") "9"]
                            [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "10") "10"]
                           [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "11") "11"]
                            [(string-contains? (list-ref (string-split (bin-month2 (first x)) " ") 0) "12") "12"]
                          [else "other"])))
       timestamp-by-type))
;Tweeets separated by month
(define jan (filter (λ (x) (string=? (second x) "1")) get-data-by-month))
(define feb (filter (λ (x) (string=? (second x) "2")) get-data-by-month))
(define march (filter (λ (x) (string=? (second x) "3")) get-data-by-month))
(define april (filter (λ (x) (string=? (second x) "4")) get-data-by-month))
(define may (filter (λ (x) (string=? (second x) "5")) get-data-by-month))
(define june (filter (λ (x) (string=? (second x) "6")) get-data-by-month))
(define july (filter (λ (x) (string=? (second x) "7")) get-data-by-month))
(define august (filter (λ (x) (string=? (second x) "8")) get-data-by-month))
(define september (filter (λ (x) (string=? (second x) "9")) get-data-by-month))
(define october (filter (λ (x) (string=? (second x) "10")) get-data-by-month))
(define november (filter (λ (x) (string=? (second x) "11")) get-data-by-month))
(define december (filter (λ (x) (string=? (second x) "12")) get-data-by-month))

(define jan-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 jan "")) )
(define feb-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 feb "")) )
(define march-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 march "")) )
(define april-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 april "")) )
(define may-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 may "")) )
(define june-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 june "")) )
(define july-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 july "")) )
(define august-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 august "")) )
(define september-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 september "")) )
(define october-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 october "")) )

(define november-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 november "")) )
(define december-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 december "")) )
(define jan-words (document->tokens  jan-tweets #:sort? #t))
(define feb-words (document->tokens  feb-tweets #:sort? #t))
(define march-words (document->tokens march-tweets #:sort? #t))
(define april-words (document->tokens april-tweets #:sort? #t))
(define may-words (document->tokens may-tweets #:sort? #t))
(define june-words (document->tokens june-tweets #:sort? #t))
(define july-words (document->tokens july-tweets #:sort? #t))
(define august-words (document->tokens august-tweets #:sort? #t))
(define september-words (document->tokens september-tweets #:sort? #t))
(define october-words (document->tokens october-tweets #:sort? #t))
(define november-words (document->tokens november-tweets #:sort? #t))
(define december-words (document->tokens december-tweets #:sort? #t))

(define jan-sentiment (list->sentiment (remove-stopwords jan-words) #:lexicon 'bing))
(define feb-sentiment (list->sentiment (remove-stopwords feb-words) #:lexicon 'bing))
(define march-sentiment (list->sentiment (remove-stopwords march-words) #:lexicon 'bing))
(define april-sentiment (list->sentiment (remove-stopwords april-words) #:lexicon 'bing))
(define june-sentiment (list->sentiment (remove-stopwords june-words) #:lexicon 'bing))
(define july-sentiment (list->sentiment (remove-stopwords july-words) #:lexicon 'bing))
(define august-sentiment (list->sentiment (remove-stopwords august-words) #:lexicon 'bing))
(define september-sentiment (list->sentiment (remove-stopwords september-words) #:lexicon 'bing))
(define  october-sentiment (list->sentiment (remove-stopwords october-words) #:lexicon 'bing))
(define november-sentiment (list->sentiment (remove-stopwords november-words) #:lexicon 'bing))
(define december-sentiment (list->sentiment (remove-stopwords december-words) #:lexicon 'bing))
(define may-sentiment (list->sentiment (remove-stopwords may-words) #:lexicon 'bing))

(define jan-negative-tokens
  (take (cdr (subset jan-sentiment 'sentiment "negative")) 10))
(define jan-positive-tokens
  (take (cdr (subset jan-sentiment 'sentiment "positive")) 10))

(define may-negative-tokens
  (take (cdr (subset may-sentiment 'sentiment "negative")) 10))
(define may-positive-tokens
  (take (cdr (subset may-sentiment 'sentiment "positive")) 10))
(define june-negative-tokens
  (take (cdr (subset june-sentiment 'sentiment "negative")) 10))
(define june-positive-tokens
  (take (cdr (subset june-sentiment 'sentiment "positive")) 10))
(define july-negative-tokens
  (take (cdr (subset july-sentiment 'sentiment "negative")) 10))
(define july-positive-tokens
  (take (cdr (subset july-sentiment 'sentiment "positive")) 10))
(define august-negative-tokens
  (take (cdr (subset august-sentiment 'sentiment "negative")) 10))
(define august-positive-tokens
  (take (cdr (subset august-sentiment 'sentiment "positive")) 10))
(define september-negative-tokens
  (take (cdr (subset september-sentiment 'sentiment "negative")) 10))
(define september-positive-tokens
  (take (cdr (subset september-sentiment 'sentiment "positive")) 10))
;Reshaping
(define n2 (map (λ (x) (list (first x) (- 0 (third x))))
	       jan-negative-tokens))
(define p2 (sort (map (λ (x) (list (first x) (third x)))
		     positive-tokens)
		(λ (x y) (< (second x) (second y)))))
;Restructuring the data for our histogram below
(define positive
  (list '( "Jan" ,(second (first jan-negative-tokens)))
        '( "Feb" ,(second (first android-quotes)))
        '( "March" ,(second (first android-quotes)))
        '( "April" ,(second (first android-quotes)))
        '( "May" ,(second (first android-quotes)))
        '( "June" ,(second (first android-quotes)))
        '( "July" ,(second (first android-quotes)))
        '( "Aug" ,(second (first android-quotes)))
        '( "Sep" ,(second (first android-quotes)))
        '( "Oct" ,(second (first android-quotes)))
        '( "Nov" ,(second (first android-quotes)))
        '("Dec" ,(second (second iphone-quotes)))))
(define negative
  (list '( "Jan" ,(second (first android-quotes)))
        '( "Feb" ,(second (first android-quotes)))
        '( "March" ,(second (first android-quotes)))
        '( "April" ,(second (first android-quotes)))
        '( "May" ,(second (first android-quotes)))
        '( "June" ,(second (first android-quotes)))
        '( "July" ,(second (first android-quotes)))
        '( "Aug" ,(second (first android-quotes)))
        '( "Sep" ,(second (first android-quotes)))
        '( "Oct" ,(second (first android-quotes)))
        '( "Nov" ,(second (first android-quotes)))
        '("Dec" ,(second (second iphone-quotes)))))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ jan-sentiment 'sentiment) ($ jan-sentiment 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
          #:label "Jan"
	 #:color "red"
	 #:line-color "MediumOrchid")
         
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))

(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ may-sentiment 'sentiment) ($ may-sentiment 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
	 #:color "green"
          #:label "May"
	 #:line-color "blue")
         
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))

(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ june-sentiment 'sentiment) ($ june-sentiment 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
          #:label "June"
	 #:color "blue"
	 #:line-color "MediumOrchid")
         
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))

(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ july-sentiment 'sentiment) ($ july-sentiment 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
	 #:color "blue"
          #:label "July"
	 #:line-color "MediumOrchid")
         
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))

(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ august-sentiment 'sentiment) ($ august-sentiment 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
	 #:color "blue"
         #:label "August"
	 #:line-color "green")
         
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ september-sentiment 'sentiment) ($ september-sentiment 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
         #:label "september"
	 #:color "blue"
	 #:line-color "red")
         
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))