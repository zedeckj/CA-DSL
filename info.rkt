#lang info

(define name "ca-dsl")

(define deps '("base"
                "rackunit-lib"
                "2htdp-typed"
                "typed/racket/stream"))

(define license 'MIT)

(define scribblings '(("scribblings/main.scrbl" () (experimental) "ca-dsl")))