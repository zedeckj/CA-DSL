#lang typed/racket

(require "../types.rkt" "../utils.rkt")
(module+ test (require typed/rackunit))

(define ALIVE-OR-DEAD-STATES : (Listof AliveOrDead) (list 'dead 'alive))

;; Utility constructor for a simple rectangular grids. Accepts a list of rows going from top to bottom, where each row is a list of states going from left to right. Ragged grids are ok. 
(: make-statemap-2d : (All (S) (-> (Listof S) * (StateMap Posn S))))
(define (make-statemap-2d . rows)
    (define ret : (StateMap Posn S) (make-hash))
    (: make-row-of-states : (Pairof Nonnegative-Integer (Listof S)) -> Void)
    (define (make-row-of-states row-with-index)
        (match-define (cons row-index row) row-with-index)
        (: handle-cell : (Pairof Nonnegative-Integer S) -> (Pairof Posn S))
        (define (handle-cell cell-with-index)
            (match-define (cons col-index state) cell-with-index)
            (cons (Posn col-index row-index) state))
        (for ([v (map handle-cell (enumerate row))]) 
            (hash-set! ret (car v) (cdr v))))
    (for ([v (enumerate rows)])
        (make-row-of-states v))
    (ann ret (Mutable-HashTable Posn S))
    ret)
(module+ test
    (check-equal? (make-statemap-2d '(0-0 1-0) '(0-1) '(0-2 1-2 2-2))
        (hash 
            (Posn 0 0) '0-0 (Posn 1 0) '1-0
            (Posn 0 1) '0-1
            (Posn 0 2) '0-2 (Posn 1 2) '1-2 (Posn 2 2) '2-2))
) 

(provide make-statemap-2d ALIVE-OR-DEAD-STATES)