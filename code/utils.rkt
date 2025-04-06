#lang typed/racket
;;; Contains general utility functions

(require "types.rkt")
(module+ test (require typed/rackunit))

;; List abstraction which augments a list with indexes, similar to `enumerate` in Python.  
(: enumerate : (All (T) ((Listof T) -> (Listof (Pairof Nonnegative-Integer T)))))
(define (enumerate lst)
    ; (define f : (Nonnegative-Integer T -> (Pairof Nonnegative-Integer T)) (lambda (i v) (cons i v)))
    (map 
        (lambda ([i : Nonnegative-Integer] [v : T]) (cons i v)) 
        (range (length lst)) lst))
(module+ test
    (check-equal? (enumerate '()) '())
    (check-equal? (enumerate (list 5)) (list (cons 0 5)))
    #;(check-equal? (enumerate (list 'one 'two 'three) #:starting-index 1) 
        (list (cons 1 'one) (cons 2 'two) (cons 3 'three)))
)

(: clamp : Integer Integer Integer -> Integer)
(define (clamp val min-val max-val)
  (cond
    [(> val max-val) max-val]
    [(< val min-val) min-val]
    [else val]))

;; Wraps numbers outside the range to be in the range, similar to integer overflow/underflow. 
(: wrap : Integer Integer Integer -> Integer)
(define (wrap val min-val max-val)
  (cond
    [(> val max-val) (wrap (- val (add1 (- max-val min-val))) min-val max-val)]
    [(< val min-val) (wrap (+ val (add1 (- max-val min-val))) min-val max-val)]
    [else val]))
(module+ test
    (check-equal? (wrap 5 1 2) 1)
    (check-equal? (wrap 3 1 2) 1)
    (check-equal? (wrap 6 -5 5) -5)
    (check-equal? (wrap 12 -5 5) 1)
    (check-equal? (wrap -12 -5 5) -1)
    (check-equal? (wrap 5 0 10) 5)
    (check-equal? (wrap 15 1 10) 5)
    (check-equal? (wrap -2 0 5) 4))
    

(define direction-offset
    (hash 'up (Posn 0 1)
        'down (Posn 0 -1)
        'left (Posn -1 0)
        'right (Posn 1 0)))

(: direction->offset : (-> Direction Posn))
(define (direction->offset d) (hash-ref direction-offset d))

(: posn-add : Posn Posn -> Posn)
(define (posn-add posn1 posn2)
  (Posn (+ (posn-x posn1) (posn-x posn2))
        (+ (posn-y posn1) (posn-y posn2))))

(: posn-scale : Integer Posn -> Posn)
(define (posn-scale n posn)
  (Posn (* n (posn-x posn))
        (* n (posn-y posn))))

(provide (all-defined-out))