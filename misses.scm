;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-

(in-module 'squad/misses)

(use-module '{logger varconfig stringfmts})
(use-module '{ofsm/graph ofsm/graph/features ofsm/graph/search})
(use-module '{squad})

(define-init %loglevel %notice%)

(module-export! '{squad/misses squad/getmiss})
(module-export! '{squad/missfn squad/testrun squad/compare})

(define default-getrank #t)

(define (squad/misses question (opts #f) (extra #f))
  (let* ((opts (if (getopt opts 'intopic)
		   (opts+ opts 'filter
		     `#[index ,squad.index 
			category ,(get question 'category)])
		   opts))
	 (result (graph/search question opts passages.nlp))
	 (misses (difference (get result 'matches)
			     (get question 'passage)))
	 (matched (overlaps? (get question 'passage)
			     (get result 'matches)))
	 (getrank (getopt opts 'getrank default-getrank))
	 (batch (getopt opts 'batch)))
    (when extra 
      (store! extra 'using (get result 'using))
      (store! extra 'bestscore (get result 'bestscore)))
    (tryif (or (exists? misses) (not matched))
      ;; Ignore cases which don't have extraneous matches and have at
      ;; least the one correct match.
      (modify-frame
	  `#[question ,question
	     passage ,(get question 'passage)
	     expected ,(get question 'passage)
	     matches ,(get result 'matches)
	     failed ,(not matched)
	     count  ,(get result 'count)
	     total  ,(get result 'total)
	     misses ,misses]
	'testid (getopt opts 'testid {})
	'scores (tryif (or (getopt opts 'getscores) (not batch))
		  (get result 'scores))
	'status (get-status result (get question 'passage))
	'rank (tryif getrank 
		(graph/search/rank result (get question 'passage)))
	'using (get result 'using)
	'bestscore (get result 'bestscore)
	'thresh (getopt result 'thresh {})))))

(define (get-status result answer)
  (choice (tryif (test result 'matches answer) 'success)
	  (tryif (test result 'best answer) 'best)
	  (tryif (identical? answer (get result 'best)) 'unique)))
       
(defambda (squad/getmiss (opts #f) (questions))
  (default! questions (?? 'type 'question))
  (let* ((question (pick-one questions))
	 (misses (squad/misses question opts))
	 (max (getopt opts 'max 42))
	 (count 0))
    (while (and (< count max) (fail? misses))
      (set! question (pick-one questions))
      (set! misses (squad/misses question opts))
      (set! count (1+ count)))
    (logwarn |GotMiss|
      "After ignoring " ($count count "match" "matches") ", found " question)
    misses))

(define (squad/missfn miss stats)
  (table-increment! stats 'matched (get miss 'count))
  (table-increment! stats 'errors (choice-size (get miss 'misses)))
  (when (fail? (get miss 'matches))
    (table-increment! stats 'nomatches))
  (if (get miss 'failed)
      (table-increment! stats 'failed)
      (table-increment! stats 'found))
  (hashset-add! (get stats 'missed) (get miss 'question))
  (do-choices (status (get miss 'status))
    (table-increment! stats status))
  (when (and (test miss 'rank) (get miss 'rank)
	     (number? (get miss 'rank)))
    (table-increment! stats 'rank (get miss 'rank))
    (table-increment! (get stats 'ranks) (get miss 'rank))))

(defambda (squad/testrun questions (stats `#[]) (opts #f))
  (unless (test stats 'ranks) (store! stats 'ranks `#[]))
  (do-choices (question questions)
    (let ((misses (squad/misses question (opt+ opts 'batch #t))))
      (table-increment! stats 'checked)
      (if misses
	  (squad/missfn misses stats)
	  (begin (table-increment! stats 'found)
	    (table-increment! (get stats 'ranks) 0)))))
  stats)

(define (opts->stats opts)
  `#[testid ,(getopt opts 'testid (symbol->string (glom "T" (random 10000))))
     opts ,(opt+ opts 'batch #t)
     ranks ,(make-hashtable)
     tried ,(make-hashset)
     missed ,(make-hashset)])

(defambda (squad/compare questions . opts)
  (let ((tests (map opts->stats (->vector opts))))
    (do-choices (question questions)
      (doseq (stats tests)
	(let* ((extra #[]) (misses (squad/misses question (get stats 'opts) extra)))
	  (unless misses
	    (table-increment! stats 'scoresum (get extra 'bestscore))
	    (table-increment! stats 'score2sum
	      (* (get extra 'bestscore)  (get extra 'bestscore)))
	    (table-increment! stats 'win_scoresum (get extra 'bestscore))
	    (table-increment! stats 'win_score2sum
	      (* (get extra 'bestscore)  (get extra 'bestscore)))
	    (add! stats 'using (get extra 'using))
	    (add! stats 'bestscore (get extra 'bestscore)))
	  (table-increment! stats 'checked)
	  (hashset-add! (get stats 'tried) question)
	  (if misses
	      (squad/missfn misses stats)
	      (begin (table-increment! stats 'found)
		(table-increment! (get stats 'ranks) 0))))))
  (let ((table (frame-create #f)))
    (doseq (stats tests)
      ;; Get the mean, can get more from the RANKS table
      (store! stats 'rank (/~ (get stats 'rank) (get stats 'checked)))
      (store! table (get stats 'testid) stats))
    table)))





