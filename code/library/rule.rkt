#lang typed/racket
(require "../types.rkt" "topologies.rkt")
(module+ test (require typed/rackunit) (require "../examples.rkt"))

;; Gets the states occurring in a given neighborhood
;; TODO Convert to a multiset
(: get-neighbors : (All (C O S) (C (StateMap C S) (Topology C O) (Neighborhood O) -> (Listof S))))
(define (get-neighbors cell state-map topology neighborhood)
  (foldr
   (lambda ([cur : (Union C Void)] [acc : (Listof S)])
     (if (or (void? cur) (not (hash-has-key? state-map cur)))
         acc
         (cons (hash-ref state-map cur) acc)))
   (list)
   (set-map neighborhood
            (lambda ([offset : O]) (topology cell offset)))))
(module+ test
  (check-equal? (get-neighbors (Posn 1 1) STATEMAP-3x3-LIVE-CROSS cartesian-topology (moore-neighborhood)) 4))

;; Checks if the number of neighbors in the specified state is one of the provided counts.
(: has-neighbors-in-state? : (All (S) (-> S (Listof S) (Listof Nonnegative-Integer) Boolean)))
(define (has-neighbors-in-state? state neighbors counts)
  (let ([found-count
         (count
          (lambda ([neighbor : S]) (equal? neighbor state))
          neighbors)])
    (ormap (lambda ([c : Nonnegative-Integer]) (= c found-count)) counts)))

(provide get-neighbors has-neighbors-in-state?)
