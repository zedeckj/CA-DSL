#lang racket


(module main racket
    (println 1)
    (module+ sub 
      (define example 3)
      (provide example)
    )
)


(require (submod "." main sub))
example