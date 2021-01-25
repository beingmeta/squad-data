#!/usr/bin/env knox
;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-

;;; SQUAD is about 20MB of text

(define (text-length f) (length (get f 'text)))
(define (text-count f) (length (segment (get f 'text))))

(config! 'cachelevel 2)
(config! 'tagger:dupstrings #t)
(config! 'log:elapsed #t)
(config! 'log:threadid #t)

(use-module '{logger varconfig logctl text/ellipsize optimize})

(define-init %loglevel (config 'loglevel %notice%))

(use-module '{texttools webtools ofsm brico})
(use-module '{ofsm/graph ofsm/graph/features ofsm/graph/index ofsm/graph/optimized})
(use-module '{squad})
(use-module '{engine fifo})
(use-module '{knodb})

;;;; NLP and indexing

(define (thread-tagger)
  (try (thread/get 'tagger)
       (let ((tagger (ofsm/start #[source #t])))
	 (thread/set! 'tagger tagger)
	 tagger)))

(define default-linkup-opts 
  `#[xterms {word root phrases concepts concepts* sensecats
	     ,(tryif (CONFIG 'USEALT #f) 'alt)}
     analyze {terms phrases marks roles quals}
     usealt (and (config 'USEALT #f) linkup/alt/soundalike)
     ground ,(config 'tagger:ground)
     context-slotids category
     save-sentences sentences
     saveparse linkup
     savetext parsetext])

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
	   (graph/index! passage linked default-linkup-opts questions.nlp))
	  (else
	   (graph/index! passage linked default-linkup-opts sentences.nlp)
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
			   'term (get match 'term)
			   'phrases (get match 'phrases)
			   'tag (get match 'tag)
			   'ishead (get match 'ishead)))))))))
    stats))

(define (read-texts (opts #f))
  (engine/run parse-text (?? 'type '{question passage})
	      (opt+ opts
		    `#[batchsize 1 logfreq 30 checkfreq 15
		       counters {words sentences terms marks}
		       logrates {words sentences}
		       checktests ,(engine/delta 'items 5000)
		       checkpoint ,{squad.pool squad.index questions.nlp
				    sentences.nlp (knodb/getindexes sentences.nlp)}])))
(define main read-texts)
