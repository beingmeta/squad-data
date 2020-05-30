;;(load-component "../../bundle.scm")
(use-module '{squad squad/misses optimize})

(config! 'tagger:index {sentences.index questions.index})

(when (config 'optimize #t)
  (optimize! '{squad squad/misses}))

