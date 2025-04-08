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

(define-states states : WireWorldState (head tail conductor insulator))

(define wireworld ;; https://conwaylife.com/wiki/OCA:WireWorld
    (moore-rule
        #:state-type WireWorldState
        [(head -> tail -> conductor)]
        [(conductor -> head) (1 2) in head]
        [(conductor -> conductor)]
        [(_ -> insulator)]))

(: wireworld-color-map : (ColorMap WireWorldState))
(define (wireworld-color-map state)
	(match state
    ;; match needs these to be quoted or the color map wont work :(
	 ['insulator BLACK]
	 ['conductor YELLOW]
	 ['head BLUE]
	 ['tail RED]))

           
(define-2d-world world : WireWorldState
    #:state-map 
    (overlay/statemaps 
        cartesian-topology  
        (Posn 0 0)
        (ann (rect-solid 50 50 insulator) (StateMap Posn WireWorldState))
        (Posn 20 20)  
        (path : WireWorldState (0 0) 
            [conductor 10 down 3 right 2 down] 
            [head 2 down] 
            [conductor 7 left])))


(define renderer : (2DRenderer WireWorldState) (make-2d-renderer wireworld-color-map))
(run world wireworld renderer)
