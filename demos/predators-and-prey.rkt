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
        [(prey -> prey) 0 in predator]
        [(empty -> predator) 2 in predator and some in prey]
        [(predator -> predator) some in prey]
        [(prey -> predator) all in prey]
        [(_ -> empty)]))

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

















;; Expansion:
#;(define predators-and-prey
     (lambda ([state-map : (StateMap Posn PredatorsAndPreyStates)]
              [topology : (Topology Posn Posn)]
              [cell : Posn])
       (let ([in-state : PredatorsAndPreyStates (hash-ref state-map cell)]
             [neighbors
              :
              (Listof PredatorsAndPreyStates)
              (get-neighbors cell state-map topology (moore-neighborhood))])
         ((lambda ([in-state : PredatorsAndPreyStates])
            ((lambda (cur-state cond fallback)
               (if (and (eq? cur-state prey) (cond)) predator (fallback cur-state)))
             in-state
             (lambda () (has-neighbors-in-state? prey neighbors (list (length neighbors))))
             (lambda ([in-state : PredatorsAndPreyStates])
               ((lambda (cur-state cond fallback)
                  (if (and (eq? cur-state prey) (cond)) prey (fallback cur-state)))
                in-state
                (lambda ()
                  (and (not
                           (has-neighbors-in-state?
                            predator
                            neighbors
                            (range 1 (add1 (set-count (moore-neighborhood))))))
                          (not
                           (has-neighbors-in-state? empty neighbors (list (length neighbors))))))
                (lambda ([in-state : PredatorsAndPreyStates])
                  ((lambda (cur-state cond fallback)
                     (if (and (eq? cur-state empty) (cond)) prey (fallback cur-state)))
                   in-state
                   (lambda ()
                     (and (has-neighbors-in-state?
                              prey
                              neighbors
                              (range 1 (add1 (set-count (moore-neighborhood)))))
                             (not
                              (has-neighbors-in-state?
                               predator
                               neighbors
                               (range 1 (add1 (set-count (moore-neighborhood))))))))
                   (lambda ([in-state : PredatorsAndPreyStates])
                     ((lambda (cur-state cond fallback)
                        (if (and (eq? cur-state predator) (cond))
                          empty
                          (fallback cur-state)))
                      in-state
                      (lambda ()
                        (not
                         (has-neighbors-in-state?
                          prey
                          neighbors
                          (range 1 (add1 (set-count (moore-neighborhood)))))))
                      (lambda ([in-state : PredatorsAndPreyStates])
                        ((lambda (cur-state cond fallback)
                           (if (cond) empty (fallback cur-state)))
                         in-state
                         (lambda () #t)
                         (lambda (state)
                           (error (format "No valid transition from state ~a" state)))))))))))))
          in-state))))
           
