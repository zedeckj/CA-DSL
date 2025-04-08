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

(define-states states : PredatorsAndPreyStates (predator prey empty))

(define predators-and-prey
    (moore-rule
        #:state-type PredatorsAndPreyStates
        [(prey -> predator) all in prey]
        [(prey -> prey) not some in predator]
        [(empty -> prey) some in prey and not some in predator]
        [(predator -> empty) not some in prey]
        [(_ -> empty)]))

           
(define-2d-world world : PredatorsAndPreyStates
     #:state-map 
        (rect-from 50 50
            (biased-random-select 
                (list (cons prey 3) (cons empty 10)))))

(define renderer : (2DRenderer PredatorsAndPreyStates) 
    (make-2d-renderer (make-default-colormap states)))
(run world predators-and-prey renderer)
