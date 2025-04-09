#lang typed/racket

(require 
    "../code/run.rkt"
    "../code/types.rkt"
    "../code/rule.rkt"
    "../code/renderer.rkt"
    "../code/library/colormaps.rkt"
    "../code/library/statemaps.rkt"
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
    (simple-2d-world 
        #:state-map (make-hash (list (cons (Posn 0 0) (ann 'conductor WireWorldState) (cons (Posn 0 1) (ann 'head WireWorldState)) (cons (Posn 0 2) (ann 'conductor WireWorldState)))))
        #;(overlay/statemaps 
            (make-wrapping-cartesian-topology -1 3 -1 3)
            (Posn 1 1) 
            (ann (rect-solid 10 10 'empty) (StateMap Posn WireWorldState))
            (Posn 0 0)
            
            #;(path : WireWorldState (0 0) 
                ['conductor 27 down]
                ['head 2 down])
            #;(Posn 1 1)  
            #;(path : WireWorldState (0 0) 
                ['conductor 5 down 3 right 2 down] 
                ['head 2 down] 
                ['conductor 7 left 3 up 5 left]
                ['head 2 down]
                ['conductor 10 down])
            #;(Posn 10 20)
            #;(path : WireWorldState (0 0)
                 ['conductor 8 left 3 down]
                 ['head 2 right]
                 ['conductor 5 right]
                 ['head 2 right] 
                 ['conductor 4 up 6 right 9 down]
                 ['head 2 down]
                 ['conductor 3 right 4 down 6 left 3 down]
                ))))


(define renderer : (2DRenderer WireWorldState) (make-2d-renderer wireworld-color-map))
(run world wireworld renderer)
