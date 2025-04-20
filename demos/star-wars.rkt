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


(define star-wars
    (moore-rule
        #:state-type Integer
        [(0 -> 1) 2 in 1]
        [(1 -> 1) (3 4 5) in 1]
        [(1 -> 2)]
        [(2 -> 3)]
        [(_ -> 0)]))

(define-2d-world world : Integer
     #:state-map 
        (rect-from 50 50
            (biased-random-select 
                (list (cons 0 2) (cons 1 1)))))

(define renderer : (2DRenderer Integer) 
    (make-2d-renderer 
        ;; colormap specified with states out of order so that the background is black
        (make-default-colormap (list 1 0 2 3 4))))
(run world star-wars renderer)











