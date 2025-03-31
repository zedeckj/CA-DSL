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

(define-type WireWorldState (U 'head 'tail 'conductor 'empty))
(define wireworld ;; https://conwaylife.com/wiki/OCA:WireWorld
    (rule
        #:cell-type Posn
        #:offset-type Posn
        #:state-type WireWorldState
        #:neighborhood (moore-neighborhood)
        [('head -> 'tail -> 'conductor)]
        [('conductor -> 'conductor)]
        [('conductor -> 'head) (1 2) in 'head]
        [default 'empty]))

           
(define world : (2DWorld WireWorldState) (random-world 50 50 (list 'head 'tail 'conductor 'empty)))
(define renderer : (2DRenderer WireWorldState) (make-2d-renderer (make-default-colormap 'head 'tail 'conductor 'empty)))
(run world wireworld renderer)

;; 

#|
StateMap/World stuff:
    random-rect 
    rect 
    2DWorld, 2DRender types

    (cartesian-world (random-rect ...))

    make-state-map

    wire function


(define wireworld ;; https://conwaylife.com/wiki/OCA:WireWorld
    (moore-rule 
        #:state-type WireWorldState
        [('head -> 'tail -> 'conductor)]
        [('conductor -> 'conductor)]
        [('conductor -> 'head) (1 2) in 'head]
        [default 'empty]))



|#