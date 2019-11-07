#!/usr/bin/env knox
;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-

;;; SQUAD is about 20MB of text

(define (text-length f) (length (get f 'text)))
(define (text-count f) (length (segment (get f 'text))))

(config! 'cachelevel 2)
(config! 'tagger:dupstrings #t)
(config! 'lexicon:prefhash #t)
(config! 'lexicon:static #t)
(config! 'log:elapsed #t)
(config! 'log:threadid #t)

(use-module '{logger varconfig logctl ellipsize optimize})

(define-init %loglevel (config 'loglevel %notice%))

(when (config 'REBUILD #f config:boolean)
  (logwarn |Rebuilding| "Removing (and replacing) existing output data files (if any)")
  (do-choices (file (pick (getfiles (get-component "data/")) has-suffix
			  '{".index" ".pool" ".rollback" ".commit" ".applied"}))
    (lognotice |Removing| file)
    (remove-file file)))

(use-module '{texttools webtools ofsm brico})
(use-module '{chopper/graph chopper/features})
(use-module '{chopper/squad})
(use-module '{engine fifo})
(use-module '{flexdb})

(config! 'tagger:xterms '{concepts* sensecat})

(define training-data
  (jsonparse (open-input-file (get-component "inputs/training.json"))))

(define (load-category cat)
  (let* ((title (get cat 'title))
	 (existing (find-frames squad.index '%id title))
	 (category (try existing
			(let ((new (frame-create squad.pool
				     'type 'category
				     'title title
				     '%id title)))
			  (index-frame squad.index new '{%id title type})
			  new))))
    (doseq (paragraph (get cat 'paragraphs) i)
      (when (exists? (find-frames squad.index 'category category 'num i))
	(logwarn |DuplicatedParagraph| 
	  (find-frames squad.index 'category category 'num i)
	  paragraph))
      (let* ((text (get paragraph 'context))
	     (passageid (glom title "["  i "]"))
	     (id (list (ellipsize text) passageid))
	     (passage (frame-create squad.pool
			'%id id 'category category
			'type 'passage 'text text
			'num i)))
	(index-frame squad.index passage '{type category num})
	(index-frame squad.index passage '%id (car id))
	(doseq (qa (get paragraph 'qas) j)
	  (let* ((text (get qa 'question))
		 (questionid (get qa 'id))
		 (question (frame-create squad.pool
			     '%id (list text passageid i j)
			     'type 'question 'category category
			     'questionid questionid
			     'passage passage
			     'text text)))
	    (doseq (answer (get qa 'answers) k)
	      (add! question 'answers
		    (frame-create #f
		      'type 'answer 'num k
		      'start (get answer 'answer_start)
		      'text (get answer 'text))))
	    (add! passage 'questions question)
	    (index-frame squad.index passage 'questions question)
	    (index-frame squad.index question
	      '{type questionid category passage})
	    (index-frame squad.index question 
	      'answers (get (get question 'answers) 'text))
	    question))
	(add! category 'passages passage)))
    category))

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

;;; Top levels

(define (load-texts (json training-data))
  (engine/run load-category (get json 'data) #[batchsize 1]))

(define (read-texts (opts #f))
  (engine/run parse-text (?? 'type '{question passage})
	      (opt+ opts
		    `#[batchsize 1 logfreq 30 checkfreq 15
		       counters {words sentences terms marks}
		       logrates {words sentences}
		       checktests ,(engine/delta 'items 5000)
		       checkpoint ,{squad.pool squad.index questions.index
				    passages.index sentences.index}])))

(define (main)
  (load-texts training-data)
  (read-texts)
  (let ((n-passages (choice-size (?? 'type 'passage)))
	(indexes (indexctl passages.index 'partitions)))
    (lognotice |Update NDOCS| n-passages " passages for: " indexes)
    (do-choices (index indexes)
      (indexctl index 'metadata 'ndocs n-passages)))
  (let ((n-sentences (choice-size (?? 'type 'sentence)))
	(indexes (indexctl sentences.index 'partitions)))
    (lognotice |Update NDOCS| n-sentences " sentences for: " indexes)
    (do-choices (index indexes)
      (indexctl index 'metadata 'ndocs n-sentences)))
  (let ((n-questions (choice-size (?? 'type 'question)))
	(indexes (indexctl questions.index 'partitions)))
    (lognotice |Update NDOCS| n-questions " questions for: " indexes)
    (do-choices (index indexes)
      (indexctl index 'metadata 'ndocs n-questions)))
  (commit))

;;; How to optimize

(when (config 'optimize #t)

  (config! 'optimize:opcodes #t)
  (config! 'optimize:special #t)
  (config! 'optimize:fcnrefs #t)
  (config! 'optimize:keepsource #t)
  (config! 'optimize:level 4)

  (optimize! '{logger varconfig logctl ellipsize optimize})
  (optimize! '{chopper/graph chopper/features})
  (optimize! '{engine fifo})
  (optimize! '{flexdb})
  (optimize!))
