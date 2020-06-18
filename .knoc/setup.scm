;;(load-component "../../bundle.scm")
(use-module '{squad squad/misses optimize})

(config! 'tagger:index {sentences.nlp questions.nlp})

(when (config 'optimize #t)
  (optimize! '{squad squad/misses}))

