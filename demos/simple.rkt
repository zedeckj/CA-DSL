#lang typed/racket

(require ca-dsl)

(define waves ;; https://conwaylife.com/wiki/OCA:WireWorld
    (moore-rule
        #:state-type Integer
        [(1 -> 1)]
        [(0 -> 0) 0 in 1]
        [(0 -> 1)]))

(: wave-color-map : (ColorMap Integer))
(define (wave-color-map state)
	(match state
	 [0 BLACK]
     [1 RED]
	 [2 ORANGE]
	 [3 YELLOW]
	 [4 GREEN]))
           

(define-2d-world world :  Integer
        #:state-map 
        (overlay/statemaps 
            (make-wrapping-cartesian-topology 20 25 20 25)
            (Posn 0 0) 
            (ann (rect-solid 40 40 0) (StateMap Posn Integer))
            (Posn 15 15)
            (rect-solid 1 1 1)))

(define renderer : (2DRenderer Integer) (make-2d-renderer wave-color-map))
(run world waves renderer)
