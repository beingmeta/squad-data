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

(use-module '{squad})
(use-module '{texttools webtools})
(use-module '{engine fifo})
(use-module '{knodb})

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

;;; Top levels

(define (load-texts (json training-data))
  (engine/run load-category (get json 'data) #[batchsize 1]))

(define (main)
  (load-texts training-data)
  (let ((n-passages (choice-size (?? 'type 'passage)))
	(indexes (indexctl passages.index 'partitions)))
    (lognotice |Update NDOCS| 
      n-passages " passages for: " 
      (do-choices (index indexes) (printout "\n\t" indexes)))
    (do-choices (index indexes)
      (indexctl index 'metadata 'ndocs n-passages)))
  (let ((n-sentences (choice-size (?? 'type 'sentence)))
	(indexes (indexctl sentences.index 'partitions)))
    (lognotice |Update NDOCS|
      n-sentences " sentences for: " 
      (do-choices (index indexes) (printout "\n\t" indexes)))
    (do-choices (index indexes)
      (indexctl index 'metadata 'ndocs n-sentences)))
  (let ((n-questions (choice-size (?? 'type 'question)))
	(indexes (indexctl questions.index 'partitions)))
    (lognotice |Update NDOCS|
      n-questions " questions for: "
      (do-choices (index indexes) (printout "\n\t" indexes)))
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

  (optimize! 'engine)

  (optimize!))
