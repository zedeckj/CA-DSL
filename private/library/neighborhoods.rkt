#lang typed/racket
(require "../types.rkt" "../utils.rkt")
(module+ test (require typed/rackunit))
(: moore-neighborhood-outline : (Positive-Integer -> (Neighborhood Posn)))
(define (moore-neighborhood-outline distance)
    ; i is the index as we traverse all of the sides simultaneously
    (: helper : Integer -> (Setof Posn))
    (define (helper i)
        (set
            (Posn distance i) ; right side
            (Posn (* -1 distance) i) ; left side
            (Posn i distance) ; top side
            (Posn i (* -1 distance)))) ; bottom side
    (define l : (Listof Integer) (range (* -1 distance) (add1 distance)))
    (define nested : (Listof (Setof Posn)) (map helper l))
    (apply set-union (first nested) (rest nested)))
(module+ test
    (check-equal? 
        (moore-neighborhood-outline 1) 
        (set 
            (Posn -1 -1) (Posn -1 0) (Posn -1 1)
            (Posn 0 -1) (Posn 0 1)
            (Posn 1 -1) (Posn 1 0) (Posn 1 1))))


(: moore-neighborhood (->* () (Positive-Integer) (Neighborhood Posn)))
(define (moore-neighborhood [distance 1])
    (define layer-indexes : (Listof Nonnegative-Integer) (range 1 (add1 distance)))
    (define nested : (Listof (Setof Posn)) 
        (map moore-neighborhood-outline (cast layer-indexes (Listof Positive-Integer))))
    (apply set-union (first nested) (rest nested)))
(module+ test 
    (check-equal? (moore-neighborhood) (set (Posn -1 -1) (Posn -1 0) (Posn -1 1)
        (Posn 0 -1) (Posn 0 1)
        (Posn 1 -1) (Posn 1 0) (Posn 1 1)))
    #;(check-equal? (moore-neighborhood 2) 
        (set-union (moore-neighborhood) 
            (list (Posn -2 -2) (Posn -2 -1) (Posn (-2)))
        )))

(provide (all-defined-out))