;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-

(in-module 'squad)

(use-module '{texttools webtools})
(use-module '{logger varconfig})
(use-module '{ofsm/graph ofsm/graph/features ofsm/graph/index ofsm/graph/search})
(use-module '{knodb})

(define-init %loglevel %notice%)

(module-export! '{squad.pool squad.index
		  sentences.pool sentences.index
		  questions.nlp passages.nlp sentences.nlp})
(module-export! '{squad/search squad/text/search})
(module-export! '{squad/questions squad/passages squad/sentences})

(define base-loc (dirname (get-component "base/squad.pool")))
(varconfig! squad:base base-loc)

(define squad-loc (dirname (get-component "data/linkups.pool")))
(varconfig! squad:loc squad-loc)

(define-init squad.pool
  (pool/ref (mkpath base-loc "squad.pool")
	    #[type knopool base @5C0AD/0 capacity #1mib
	      create #t]))
(define-init squad.linkups
  (pool/ref (mkpath squad-loc "linkups.pool")
	    #[adjunct linkup
	      type knopool base @5C0AD/0 capacity #1mib
	      create #t]))
(adjunct! squad.pool 'linkup squad.linkups)

(define-init squad.index
  (knodb/ref (mkpath base-loc "squad.index")
	     #[type knoindex capacity (* 8 #1mib) create #t
	       background #t]))

;;; For saving passage -> sentences mapping
(define-init squad.sentences
  (pool/ref (mkpath squad-loc "sentences.adjunct.pool")
	    #[adjunct sentences
	      type knopool base @5C0AD/0 capacity #1mib
	      create #t]))
(adjunct! squad.pool 'sentences squad.sentences)

;; (define-init sentences.linkups
;;   (pool/ref (mkpath squad-loc "sentences.linkups.pool")
;; 	    #[adjunct linkup
;; 	      type knopool base @5C0AD5/0 capacity #1mib
;; 	      create #t]))
;; (adjunct! sentences.pool 'linkup sentences.linkups)

(define nl-slots '{terms marks roles phrases quals})

(define (make-nlp-index prefix (opts #f))
  (graph-db (mkpath squad-loc prefix) nl-slots (opt+ opts 'etc #t 'context #t)))

(define-init questions.nlp (make-nlp-index "questions" #[sentences #f]))
(let ((ndocs (choice-size (?? 'type 'question))))
  (indexctl {questions.nlp (indexctl questions.nlp 'partitions)}
	    'props 'ndocs ndocs))

(define-init sentences.nlp 
  (make-nlp-index "sentences"
		  #[sentences #[adjuncts #[linkups #[type kpool]]]]))
(let ((index (pool/getindex sentences.nlp)))
  (indexctl {index (indexctl index 'partitions)}
	    'props 'ndocs (choice-size (find-frames index 'type 'sentence))))

(define name->index
  `#[sentences ,sentences.nlp
     questions ,questions.nlp])

(define (opts->index opts (dflt sentences.nlp) (index))
  (set! index (getopt opts 'index (getopt opts 'domain)))
  (when (symbol? index)
    (set! index (try (get name->index index) 
		     (get name->index 'default)
		     #f)))
  (or index sentences.nlp))

(define (squad/search q (opts #f) (index))
  (default! index (opts->index opts))
  (cond ((string? q) (linkup q opts))
	((oid? q) (try (get q 'linkup)
		       (linkup (get q 'text) opts))))
  (if (and (getopt opts 'intopic #f) (test q 'category))
      (graph/search q opts index)
      (graph/search q (opt+ opts 'filter `#[index ,squad.index category (get q 'category)])
		    index)))

(define (squad/text/search q (opts #f) (index))
  (default! index (opts->index opts))
  (cond ((string? q) (linkup q opts))
	((oid? q) (try (get q 'linkup)
		       (linkup (get q 'text) opts))))
  (if (and (getopt opts 'intopic #f) (test q 'category))
      (graph/search q opts index)
      (graph/search q (opt+ opts 
			  'filter `#[index ,squad.index category (get q 'category)]
			  'features- '{marks roles tuples})
		    index)))

(define (squad/text/search q (opts #f) (index))
  (default! index (opts->index opts))
  (cond ((string? q) (linkup q opts))
	((oid? q) (try (get q 'linkup) (linkup (get q 'text) opts))))
  (graph/search q `(opts+ opts 'exclude {marks roles tuples}) index))

;;; Getting stuff

(define (squad/questions (n #f))
  (if n (pick-n (?? 'type 'question) n) (?? 'type 'question)))
(define (squad/passages (n #f))
  (if n (pick-n (?? 'type 'passage) n) (?? 'type 'passage)))
(define (squad/sentences (n #f))
  (if n (pick-n (?? 'type 'sentence) n) (?? 'type 'sentence)))
