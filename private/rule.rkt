#lang typed/racket
(require "types.rkt" "world.rkt" "examples.rkt" racket/syntax syntax/parse
         racket/bool (for-syntax racket/syntax syntax/parse))
(module+ test (require syntax/macro-testing typed/rackunit "examples.rkt")
  (require syntax/macro-testing))

;; Gets the states occurring in a given neighborhood
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

;; Checks if the number of neighbors in the specified state is one of the provided counts.
(: has-neighbors-in-state? : (All (S) (-> S (Listof S) (Listof Nonnegative-Integer) Boolean)))
(define (has-neighbors-in-state? state neighbors counts)
  (let ([found-count
         (count
          (lambda ([neighbor : S]) (equal? neighbor state))
          neighbors)])
    (ormap (lambda ([c : Nonnegative-Integer]) (= c found-count)) counts)))


;; Parses a number or DSL keyword which is specified as an allowed number of neighbors
(define-syntax (parse-count stx)
  (syntax-parse stx
    [(_ neighbors:expr _ (~datum all))
     #'(list (length neighbors))]
    [(_ _ neighborhood-len:expr (~datum some))
     #'(range 1 (add1 neighborhood-len))]
    [(_ _ _ count:expr)
     #'(list (ann count Nonnegative-Integer))]))



(begin-for-syntax
  ;; https://en.cppreference.com/w/c/language/operator_precedence

  (define-syntax-class and-nand
    #:description "and/nand"
    (pattern (~or (~datum and) (~datum nand))))

  (define-syntax-class or-nor
    #:description "or/nor"
    (pattern (~or (~datum or) (~datum nor))))

  (define-syntax-class xor-class
    #:description "xor"
    (pattern (~datum xor)))

  (define-syntax-class implies-class
    #:description "implies"
    (pattern (~datum implies))))


;; Translates the condition portion of a clause into an expression that will evaluate into a boolean
(define-syntax (parse-condition stx)
  (syntax-parse stx
    [(~or
      (_ info-bundle:expr cond-tokens1:expr ...+ operator:implies-class cond-tokens2:expr ...+)
      (_ info-bundle:expr cond-tokens1:expr ...+ operator:or-nor cond-tokens2:expr ...+)
      (_ info-bundle:expr cond-tokens1:expr ...+ operator:xor-class cond-tokens2:expr ...+)
      (_ info-bundle:expr cond-tokens1:expr ...+ operator:and-nand cond-tokens2:expr ...+))
     #'(operator
        (parse-condition info-bundle cond-tokens1 ...)
        (parse-condition info-bundle cond-tokens2 ...))]

    [(_ info-bundle:expr (~datum not) cond-tokens:expr ...)
     #'(not (parse-condition info-bundle cond-tokens ...) )]

    [(_ (neighbors:expr neighborhood-len:expr state-type:id) (count:expr ...+) (~datum in) state:expr)
     #'(let
           ([count-list : (Listof Nonnegative-Integer) (list count ...)])
         (has-neighbors-in-state? (ann state state-type) neighbors count-list))]
    [(_ (neighbors:expr neighborhood-len:expr state-type:id) count:expr (~datum in) state:expr)
     #'(has-neighbors-in-state? (ann state state-type) neighbors (parse-count neighbors neighborhood-len count))]
    [(_ _) #'#t]))

;; 3 in prey and 0 in predator

;; EXPANDS TO

#; (and (has-neighbors-in-state?
         (ann prey PredatorsAndPreyState)
         neighbors
         (list (ann 3 Nonnegative-Integer)))
        (has-neighbors-in-state?
         (ann predator PredatorsAndPreyState)
         neighbors
         (list (ann 0 Nonnegative-Integer))))

(module+ test
  (check-equal? (parse-condition ('() 0 Symbol) not 2 in 'alive) #t)
  (check-equal? (parse-condition ('() 0 Symbol) 0 in 'alive) #t)
  (check-equal? (parse-condition ('() 0 Symbol) not 2 in 'alive and 0 in 'dead) #t)
  (check-equal? (parse-condition ('() 0 Symbol) 4 in 'alive and 3 in 'dead or 0 in 'alive) #t)
  (check-equal? (parse-condition ('() 0 Symbol) 0 in 'alive or 4 in 'alive and 3 in 'dead ) #t)
  (check-equal? (parse-condition ('() 0 Symbol) 0 in 'alive xor 4 in 'alive implies 3 in 'dead ) #f)
  )

;; Composes a clause by parsing a transition expression of the form (a -> b) into roughly
;; (if (and (eq? cur-state a) condition) b fallback), while being given a condition that has already
;; been parsed into an expression that will evaluate into a boolean.
(define-syntax (compose-clause stx)
  (syntax-parse stx
    [(_ ((~datum _) (~datum ->) to-state:expr) _ (_ _ state-type:id) condition:expr fallback:expr)
     #'(if condition (ann to-state state-type) fallback)]
    [(_ (from-state:expr (~datum ->) to-state:expr) cur-state:expr (_ _ state-type:id) condition:expr fallback:expr)
     #'(if (and (eq? cur-state (ann from-state state-type)) condition)
           (ann to-state state-type)
           fallback)]))

#;[(empty -> prey) 3 in prey and 0 in predator]

;; EXPANDS TO

#;(if (and
       (eq? in-state (ann empty PredatorsAndPreyState))
       (and
        (has-neighbors-in-state? (ann prey PredatorsAndPreyState) neighbors (list (ann 3 Nonnegative-Integer)))
        (has-neighbors-in-state? (ann predator PredatorsAndPreyState) neighbors (list (ann 0 Nonnegative-Integer)))))
      prey
      ...)

;; Helper function for parse-clauses that implements functionality for chained state transitions
;; such as #'(a -> b -> c) by deconstructing this syntax into a list of binary transitions
;; (so the above example becomes [#'(a -> b) #'(b -> c)])
;; Syntax -> Listof Syntax
(define-for-syntax (deconstruct-transition stx)
  (syntax-parse stx
    [(from-state:expr (~datum ->) to-state:expr) (list stx)]
    [(from-state:expr (~datum ->) to-state:expr more:expr ...+)
     (append
      (deconstruct-transition #'(to-state more ...))
      (list #'(from-state -> to-state)))]))

;; parse-clauses recursively expands each branch given to the rule macro as nested if expressions
;; which evaluate to the "out" state for a cell
(define-syntax (parse-clauses stx)
  (syntax-parse stx
    [(_ info-bundle:expr cur-state:expr [transition:expr condition-token:expr ...]
        clauses ...)
     (define/syntax-parse
       (current-simple-transition more-simple-transitions ...)
       (deconstruct-transition #'transition))
     #'(compose-clause
        current-simple-transition
        cur-state
        info-bundle
        (parse-condition info-bundle condition-token ...)
        (parse-clauses
         info-bundle
         cur-state
         (more-simple-transitions condition-token ...) ...
         clauses ...)
        )]

    [(_ _ cur-state)
     #'(error (format "No valid transition from state ~a" cur-state))]))

;; Example expansion

#; (parse-clauses ((list 1 2 3) 3 Integer) 0
                  [(1 -> 1)]
                  [(0 -> 0) 0 in 1]
                  [(0 -> 1)])

#; (if (and (eq? 0 (ann 1 Integer)) #t)
       (ann 1 Integer)
       (if (and (eq? 0 (ann 0 Integer)) (has-neighbors-in-state? (ann 1 Integer) (list 1 2 3) (list (ann 0 Nonnegative-Integer))))
           (ann 0 Integer)
           (if (and (eq? 0 (ann 0 Integer)) #t) (ann 1 Integer) (error (format "No valid transition from state ~a" 0)))))



;; Example Expansion
#;[(1 -> 1)] #;[(0 -> 0) 0 in 1] #;[(0 -> 1)]

#; (if (and (eq? 0 (ann 1 Integer)) #t)
       (ann 1 Integer)
       (if (and (eq? 0 (ann 0 Integer)) (has-neighbors-in-state? (ann 1 Integer) (list 1 2 3) (list (ann 0 Nonnegative-Integer))))
           (ann 0 Integer)
           (if (and (eq? 0 (ann 0 Integer)) #t) (ann 1 Integer) (error (format "No valid transition from state ~a" 0)))))




;; Main macro for declaring a Rule for a cellular automata in the most general case the DSL supports
;; Takes in keyword arguments for types, the neighborhood, and at least one conditional transition
;; ("clauses"), and returns syntax for a lambda that takes in a statemap, a topology, and a cell, and
;; returns the resulting state for that cell after a time step. (This type is defined as a Rule).
;; Syntax Custom -> Syntax Rule
(define-syntax (rule stx)
  (syntax-parse stx
    [(_
      #:cell-type cell-type:id
      #:offset-type offset-type:id
      #:state-type state-type:id
      #:neighborhood neighborhood:expr
      clauses:expr ...+)
     #`(lambda
           ([state-map : (StateMap cell-type state-type)]
            [topology : (Topology cell-type offset-type)]
            [cell : cell-type])
         (let ([in-state : state-type (hash-ref state-map cell)]
               [neighbors : (Listof state-type) (get-neighbors cell state-map topology neighborhood)])
           (parse-clauses (neighbors (set-count neighborhood) state-type) in-state clauses ...)))]))

#;(rule
   #:cell-type Posn
   #:offset-type Posn
   #:state-type PredatorsAndPreyState
   #:neighborhood (moore-neighborhood)
   [(empty -> prey) 3 in prey and 0 in predator]
   [(prey -> predator) all in prey]
   [(prey -> prey) 0 in predator]
   [(empty -> predator) 2 in predator and some in prey]
   [(predator -> predator) some in prey]
   [(_ -> empty)])

;; EXPANDS TO

#;(lambda ([state-map : (StateMap Posn PredatorsAndPreyState)] [topology : (Topology Posn Posn)] [cell : Posn])
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
                          (if #t (ann empty PredatorsAndPreyState) (error (format "No valid transition from state ~a" in-state))))))))))

;; Shorthand macro that expands to `rule` for the common usecase of making a Rule with Posn cell and offset types, and a neighborhood consisting of the Moore neighborhood with a radius of 1
(define-syntax (moore-rule stx)
  (syntax-parse stx
    [(_
      #:state-type state-type:id
      clauses:expr ...+)
     #'(rule
        #:cell-type Posn
        #:offset-type Posn
        #:state-type state-type
        #:neighborhood (moore-neighborhood)
        clauses ...)]))

;; Shorthand that expands to `rule` for making a rule with alive and dead states with a default state of dead and uses a moore neighborhood with cells and offsets represented as Posn
(define-syntax (lifelike stx)
  (syntax-parse stx
    [(_
      [(~datum born) born-cond:expr ...+]
      [(~datum survive) survive-cond:expr ...+])
     #'(moore-rule
        #:state-type AliveOrDead
        [('dead -> 'alive) (born-cond ...) in 'alive]
        [('alive -> 'alive) (survive-cond ...) in 'alive]
        [(_ -> 'dead)])]))

(provide lifelike rule moore-rule)
