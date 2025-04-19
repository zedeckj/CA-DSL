#lang info

(define name "ca-dsl")

(define deps '("base"
                "rackunit-typed"
                "2htdp-typed"
                "typed-racket-lib"
                "typed-racket-stream"
                "racket-doc"
                "rackunit-lib"
                "scribble-lib"
                "scheme-lib"
                "at-exp-lib"
                "htdp-lib"
                "htdp-doc"
                "scheme-lib"))

(define license 'MIT)

(define scribblings '(("scribblings/main.scrbl" () (experimental) "ca-dsl")))