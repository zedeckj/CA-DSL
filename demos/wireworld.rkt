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

(define-type WireWorldState (U 'head 'tail 'conductor 'empty))
(define wireworld ;; https://conwaylife.com/wiki/OCA:WireWorld
    (moore-rule
        #:state-type WireWorldState
        [('head -> 'tail -> 'conductor)]
        [('conductor -> 'head) (1 2) in 'head]
        [('conductor -> 'conductor)]
        [(_ -> 'empty)]))

(: wireworld-color-map : (ColorMap WireWorldState))
(define (wireworld-color-map state)
	(match state
	 ['empty BLACK]
	 ['conductor YELLOW]
	 ['head BLUE]
	 ['tail RED]))
           

(define world : (2DWorld WireWorldState) 
    (simple-world 
        #:statemap 
        (overlay/statemaps 
            cartesian-topology  
            (Posn -10 -10) 
            (rect-solid 20 20 (ann 'empty WireWorldState))
            (Posn 0 0)  
            (path : WireWorldState (0 0) 
                ['conductor 5 down 3 right 2 down] 
                ['head 2 down] 
                ['conductor 7 left]))))


(define renderer : (2DRenderer WireWorldState) (make-2d-renderer wireworld-color-map))
(run world wireworld renderer)
