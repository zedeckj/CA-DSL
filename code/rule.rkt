#lang typed/racket
(require (for-syntax racket/syntax syntax/parse))
(require "types.rkt" "library/topologies.rkt" "library/neighborhoods.rkt")
(module+ test (require syntax/macro-testing typed/rackunit "examples.rkt"))

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

(define-syntax (parse-condition stx)
  (syntax-parse stx
    [(_ neighbors:expr (count:expr ...+) (~datum in) state:expr) #'((inst has-neighbors-in-state? AliveOrDead) state neighbors (list count ...))]
    [(_ neighbors:expr count:expr (~datum in) state:expr) #'((inst has-neighbors-in-state? AliveOrDead) state neighbors (list count))]))

(define-syntax (parse-clauses stx)
  (syntax-parse stx
    [(_ neighbors:id state-type:id state:id 
      ((in-state:expr (~datum ->) out-state:expr) condition:expr ...+) clauses ...+)
      #'(let ([state-name : state-type out-state]) ; for type checking, even if shortcircuiting
      (if 
        (and (parse-condition neighbors condition ...) (equal? in-state state))
        state-name 
        (parse-clauses neighbors state-type state clauses ...)))]
      
    [(_ _ state-type:id _ ((~datum default) out-state:expr)) 
      #'(let ([state-name : state-type out-state]) state-name)]))

;; Main macro for declaring a Rule for a cellular automata
(define-syntax (rule stx)
  (syntax-parse stx
    [(_
      #:state-type state-type:id
      #:cell-type cell-type:id
      #:offset-type offset-type:id
      #:neighborhood neighborhood:expr
      clauses:expr ...)
        #'(lambda
                ([state-map : (StateMap cell-type state-type)]
                 [topology : (Topology cell-type offset-type)])
            (hash-map/copy state-map 
              (lambda ([cell : cell-type]
                       [in-state : state-type])
                (values cell
                  (let ([neighbors : (Listof state-type) (get-neighbors cell state-map topology neighborhood)])
                    (parse-clauses neighbors state-type in-state clauses ...))))))]))

;; Shorthand for making a rule with alive and dead states with a default state of dead and uses a moore neighborhood with cells and offsets represented as Posn
(define-syntax (lifelike stx)
  (syntax-parse stx
    [(_ 
    [(~datum born) born-cond:expr ...+]
    [(~datum survive) survive-cond:expr ...+])
    #'(rule 
    #:state-type AliveOrDead
    #:cell-type Posn
    #:offset-type Posn 
    #:neighborhood (moore-neighborhood)
    [('dead -> 'alive) (born-cond ...) in 'alive]
    [('alive -> 'alive) (survive-cond ...) in 'alive] 
    [default 'dead])]))

(provide lifelike rule)
