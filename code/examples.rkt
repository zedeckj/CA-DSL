#lang typed/racket
(require "types.rkt" "utils.rkt" "library/statemaps.rkt" "library/topologies.rkt")

;; (StateMap C S) examples
(define STATEMAP-EMPTY : (StateMap Any Any) (make-hash ))
;; Statemap with (0,0) -> 1 and (1,0) -> 1
(define STATEMAP-0-1 (ann (make-hash (list [cons (Posn 0 0) 0] [cons (Posn 1 0) 1])) (StateMap Posn Integer)))
(define STATEMAP-3x3-LIVE-CROSS : (StateMap Posn AliveOrDead) 
    (make-hash 
        (list 
        [cons (Posn 0 0) 'alive] [cons (Posn 1 0) 'dead] [cons (Posn 2 0) 'alive]
        [cons (Posn 1 0) 'dead] [cons (Posn 1 1) 'alive] [cons (Posn 2 1) 'dead]
        [cons (Posn 2 0) 'alive] [cons (Posn 1 0) 'dead] [cons (Posn 2 0) 'alive])))
(define STATEMAP-3x3-CENTER-ALIVE : (StateMap Posn AliveOrDead)
    (make-statemap-2d 
    (list 'dead 'dead 'dead)
    (list 'dead 'alive 'dead)
    (list 'dead 'dead 'dead)
))
(define STATEMAP-3x3-ALL-DEAD : (StateMap Posn AliveOrDead) 
    (make-statemap-2d
    (list 'dead 'dead 'dead)
    (list 'dead 'dead 'dead)
    (list 'dead 'dead 'dead)
))

(: ACTIVEFILTER-NOT-ORIGIN : (ActiveFilter Posn))
    (define (ACTIVEFILTER-NOT-ORIGIN pos)
        (not (equal? pos (Posn 0 0))))

(define WORLD-CART-2CELL : (World Posn Posn Integer) (World STATEMAP-0-1 cartesian-topology (lambda (c) #t)))

(provide (all-defined-out))

