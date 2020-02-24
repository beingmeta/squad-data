;;;; NLP and indexing

(define (thread-tagger)
  (try (thread/get 'tagger)
       (let ((tagger (ofsm/start #[source #t])))
	 (thread/set! 'tagger tagger)
	 tagger)))

(define default-linkup-opts 
  `#[xterms {word root phrases concepts concepts* sensecats
	       ,(tryif (CONFIG 'USEALT #f) 'alt)}
     usealt (and (config 'USEALT #f) linkup/alt/soundalike)
     ground ,(config 'tagger:ground)])

(define (parse-text passage (stats (frame-create #f)))
  (let* ((tagger (thread-tagger))
	 (text (get passage 'text))
	 (tagged (tagtext text tagger))
	 (linked (linkup tagged default-linkup-opts))
	 (sentences #f))
    (cond ((test passage 'type 'question)
	   (store! passage 'linkup 
		   (vector->compound 
		    (->vector (apply append (map ->vector linked)))
		    'linkup))
	   (doseq (word (get passage 'linkup))
	     (store! word '{passage sentence} passage))
	   (linkup/index! passage questions.index default-linkup-opts stats))
	  (else
	   (store! passage 'sentences
		   (->vector
		    (forseq (sentence linked i)
		      (make-sentence sentence passage
				     squad.pool
				     squad.index
				     sentences.index
				     passages.index
				     stats
				     i))))
	   (do-choices (question (get passage 'questions))
	     (do-choices (answer (get question 'answers))
	       (let ((match (linkup/getoff linked (get answer 'start))))
		 (when match
		   (add! answer 'match
			 (frame-create #f 
			   'phrase (get match 'phrase)
			   'sentence (get match 'sentence)
			   'passage (get match 'passage)
			   'wordno (get match 'wordno)
			   'term (get match 'term) 'phrases (get match 'phrases)
			   'tag (get match 'tag) 'ishead (get match 'ishead)))))))))
    stats))

(define (make-sentence sentence passage q.pool q.index
		       sentences.index
		       passages.index
		       stats
		       (i #f))
  (let* ((text (stringout 
		 (doseq (frame sentence i)
		   (if (test frame 'source)
		       (printout (get frame 'source))
		       (printout (if (> i 0) " ") (get frame 'term) )))))
	 (frame (frame-create squad.pool
		  'type 'sentence '%id (ellipsize text)
		  'passage passage
		  'text text
		  'linkup sentence
		  'sentence_no (tryif i i))))
    (add! (elts sentence) 'sentence frame)
    (add! (elts sentence) 'passage passage)
    (index-frame q.index frame '{type passage})
    (linkup/index! frame 
		   (qc (cons sentences.index frame)
		       (cons passages.index passage))
		 default-linkup-opts
		 stats)
    frame))

