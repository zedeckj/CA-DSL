#lang typed/racket
(require (for-syntax racket/syntax syntax/parse))



(define-syntax (parse-clauses stx)
  (syntax-parse stx
    [(_ state ((in-state:expr -> out-state:expr) condition:expr) clauses ...+)
     #'(if (and condition (equal? in-state state)) out-state (parse-clauses state clauses ...))]
    [(_ _ ((~datum default) out-state:expr)) #'out-state]))

(define-syntax (rule stx)
  (syntax-parse stx
    [(_
      #:state-type state-type:id
      clauses:expr ...)

        #'(lambda ([state : state-type]) (parse-clauses state clauses ...))]))

(define test
  (rule
   #:state-type Integer
   [(0 -> 1) #f]
   [(1 -> 1) #t]
   [default 0]))

