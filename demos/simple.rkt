#lang typed/racket

(require 
    "../code/run.rkt"
    "../code/types.rkt"
    "../code/rule.rkt"
    "../code/renderer.rkt"
    "../code/library/colormaps.rkt"
    "../code/library/topologies.rkt"
    "../code/library/neighborhoods.rkt"
    "../code/library/statemaps.rkt"
    (for-syntax syntax/parse syntax/macro-testing))
(module+ test (require typed/rackunit))

(define waves ;; https://conwaylife.com/wiki/OCA:WireWorld
    (moore-rule
        #:state-type Nonnegative-Integer
        [( 1 -> 1)]
        [(0 -> 1) (1 2 3 4 5 6 7 8 9) in 1]))

(: wave-color-map : (ColorMap Nonnegative-Integer))
(define (wave-color-map state)
	(match state
	 [0 BLACK]
     [1 RED]
	 [2 ORANGE]
	 [3 YELLOW]
	 [4 GREEN]))
           

(define world : (2DWorld Nonnegative-Integer) 
    (simple-2d-world 
        #:state-map 
        (overlay/statemaps 
            (make-wrapping-cartesian-topology 0 20 0 10)
            (Posn 0 0) 
            (ann (rect-solid 10 10 0) (StateMap Posn Nonnegative-Integer))
            (Posn 0 0)
            (rect-solid 1 1 0))))


(define renderer : (2DRenderer Nonnegative-Integer) (make-2d-renderer wave-color-map))
(run world waves renderer)
