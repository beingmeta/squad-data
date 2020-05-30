;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-

(in-module 'squad)

(use-module '{texttools webtools})
(use-module '{logger varconfig})
(use-module '{ofsm/graph ofsm/graph/features ofsm/graph/search})
(use-module '{knodb})

(define-init %loglevel %notice%)

(module-export! '{squad.pool squad.index
		  questions.index 
		  passages.index
		  sentences.pool
		  sentences.index})
(module-export! '{squad/search squad/text/search})
(module-export! '{squad/questions squad/passages squad/sentences})

(define base-loc (dirname (get-component "base/squad.pool")))
(varconfig! squad:base base-loc)

(define squad-loc (dirname (get-component "data/squad.pool")))
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
(define-init squad.sentences.adjunct
  (pool/ref (mkpath squad-loc "sentences.adjunct.pool")
	    #[adjunct sentences
	      type knopool base @5C0AD/0 capacity #1mib
	      create #t]))
(adjunct! squad.pool 'linkup squad.linkups)

(define-init squad.index
  (knodb/ref (mkpath base-loc "squad.index")
	     #[type knoindex capacity (* 8 #1mib) create #t
	       background #t]))

(define-init sentences.pool
  (pool/ref (mkpath squad-loc "sentences.pool")
	    #[type knopool base @5C0AD5/0 capacity #1mib
	      create #t]))

(define nl-slots  '{terms marks roles quals})

(define (make-combo-index prefix)
  (make-aggregate-index
   {(for-choices (slotid nl-slots)
       (knodb/ref (mkpath squad-loc (glom prefix "_" (downcase slotid) ".index"))
		  `#[type knoindex keyslot ,slotid capacity (* 12 #1mib) create #t]))
    (knodb/ref (mkpath squad-loc (glom prefix "_etc"  ".index"))
	       `#[type knoindex capacity #1mib create #t])}
   #[register #t]))

(define-init questions.index (make-combo-index "questions"))
(indexctl {questions.index (indexctl questions.index 'partitions)}
	  'props 'ndocs (choice-size (?? 'type 'question)))

(define-init passages.index (make-combo-index "passages"))
(indexctl {passages.index (indexctl passages.index 'partitions)}
	  'props 'ndocs (choice-size (?? 'type 'passage)))

(define-init sentences.index (make-combo-index "sentences"))
(indexctl {(indexctl sentences.index 'partitions) sentences.index}
	  'props 'ndocs (choice-size (?? 'type 'sentence)))

(define name->index
  `#[sentences ,sentences.index
     passages ,passages.index
     questions ,questions.index])

(define (opts->index opts (dflt passages.index) (index))
  (set! index (getopt opts 'index (getopt opts 'domain)))
  (when (symbol? index)
    (set! index (try (get name->index index) 
		     (get name->index 'default)
		     #f)))
  (or index passages.index))

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


