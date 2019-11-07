;;(load-component "../../bundle.scm")
(use-module '{chopper/squad chopper/squad/misses})

(config! 'tagger:index {sentences.index questions.index})

(when (config 'optimize #t)
  (optimize! '{chopper/squad chopper/squad/misses}))
