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

(define-states states : PredatorsAndPreyState (empty prey predator))

(define predators-and-prey
    (moore-rule
        #:state-type PredatorsAndPreyState
        [(empty -> prey) 3 in prey and 0 in predator]
        [(prey -> predator) all in prey]
        [(prey -> prey) 0 in predator]
        [(empty -> predator) 2 in predator and some in prey]
        [(predator -> predator) some in prey]
        [(_ -> empty)]))


#;(define predators-and-prey
     (lambda ([state-map : (StateMap Posn PredatorsAndPreyState)] [topology : (Topology Posn Posn)] [cell : Posn])
       (let ([in-state : PredatorsAndPreyState (hash-ref state-map cell)]
             [neighbors : (Listof PredatorsAndPreyState) (get-neighbors cell state-map topology (moore-neighborhood))])
         (if (and (eq? in-state (ann empty PredatorsAndPreyState))
                  (and (has-neighbors-in-state? (ann prey PredatorsAndPreyState) neighbors (list (ann 3 Nonnegative-Integer)))
                       (has-neighbors-in-state? (ann predator PredatorsAndPreyState) neighbors (list (ann 0 Nonnegative-Integer)))))
           (ann prey PredatorsAndPreyState)
           (if (and (eq? in-state (ann prey PredatorsAndPreyState))
                          (has-neighbors-in-state? (ann prey PredatorsAndPreyState) neighbors (list (length neighbors))))
             (ann predator PredatorsAndPreyState)
             (if (and (eq? in-state (ann prey PredatorsAndPreyState))
                            (has-neighbors-in-state? (ann predator PredatorsAndPreyState) neighbors (list (ann 0 Nonnegative-Integer))))
               (ann prey PredatorsAndPreyState)
               (if (and (eq? in-state (ann empty PredatorsAndPreyState))
                              (and (has-neighbors-in-state? (ann predator PredatorsAndPreyState) neighbors (list (ann 2 Nonnegative-Integer)))
                                   (has-neighbors-in-state?
                                    (ann prey PredatorsAndPreyState)
                                    neighbors
                                    (range 1 (add1 (set-count (moore-neighborhood)))))))
                 (ann predator PredatorsAndPreyState)
                 (if (and (eq? in-state (ann predator PredatorsAndPreyState))
                                (has-neighbors-in-state?
                                 (ann prey PredatorsAndPreyState)
                                 neighbors
                                 (range 1 (add1 (set-count (moore-neighborhood))))))
                   (ann predator PredatorsAndPreyState)
                   (if #t (ann empty PredatorsAndPreyState) (error (format "No valid transition from state ~a" in-state)))))))))))




#| taken from old C project
B3&0/
S0/
{0->2}2&12345678/
{2->0}0/
{1->2}8
|#

(define-2d-world world : PredatorsAndPreyState
     #:state-map 
        (rect-from 50 50
            (biased-random-select 
                (list (cons prey 2) (cons predator 1) (cons empty 5)))))

(define renderer : (2DRenderer PredatorsAndPreyState) 
    (make-2d-renderer (make-default-colormap states)))
(run world predators-and-prey renderer)











