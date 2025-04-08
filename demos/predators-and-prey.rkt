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

(define-states states : PredatorsAndPreyStates ('predator 'prey 'empty))

(define predators-and-prey
    (moore-rule
        #:state-type PredatorsAndPreyStates
        [('prey -> 'predator) 8 in 'prey]
        [('prey -> 'prey) 0 in 'predator]
        [('empty -> 'prey) (1 2 3 4 5 6 7 8) in 'prey]
        [('predator -> 'empty) 0 in 'prey]
        [(_ -> 'empty)]))

           
(define world : (2DWorld PredatorsAndPreyStates) 
    (simple-2d-world #:state-map 
        (ann 
            (rect-from 50 50
                (biased-random-select 
                    (ann (list (cons 'prey 3) (cons 'empty 10)) 
                        (Listof (Pairof PredatorsAndPreyStates Nonnegative-Integer)))))
            (StateMap Posn PredatorsAndPreyStates))))

(define renderer : (2DRenderer PredatorsAndPreyStates) 
    (make-2d-renderer (make-default-colormap states)))
(run world predators-and-prey renderer)
