#lang typed/racket
(require "types.rkt" "library/topologies.rkt" "library/neighborhoods.rkt" "examples.rkt" racket/syntax syntax/parse
         (for-syntax racket/syntax syntax/parse))
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


;; Processes the neighbors
(define-syntax (parse-count stx)
  (syntax-parse stx
    [(_ neighbors:expr _ (~datum all))
     #'(list (length neighbors))]
    [(_ _ neighborhood-len:expr (~datum some))
     #'(range 1 (add1 neighborhood-len))]
     [(_ _ _ count:expr)
      #'(list (ann count Nonnegative-Integer))]))


(define-syntax (parse-condition stx)
  (syntax-parse stx
    [(_ neighbors:expr neighborhood-len:expr (count:expr ...+) (~datum in) state:expr)
     #'(let
           ([count-list : (Listof Nonnegative-Integer) (list count ...)])
         (has-neighbors-in-state? state neighbors count-list))]
    [(_ neighbors:expr neighborhood-len:expr count:expr (~datum in) state:expr)
        #'(has-neighbors-in-state? state neighbors (parse-count neighbors neighborhood-len count))]
    [(_ neighbors:expr neighborhood-len) #'#t]))


(define-syntax (parse-compound-cond stx)
	(syntax-parse stx
		[(_ neighbors:expr neighborhood-len:expr 
          cond-tokens1:expr ... (~datum or) cond-tokens2:expr ...)
			#'(or 
					(parse-compound-cond neighbors neighborhood-len cond-tokens1 ...) 
					(parse-compound-cond neighbors neighborhood-len cond-tokens2 ...))]
		[(_ neighbors:expr neighborhood-len:expr 
          cond-tokens1:expr ... (~datum and) cond-tokens2:expr ...)
			#'(and 
					(parse-compound-cond neighbors neighborhood-len cond-tokens1 ...) 
					(parse-compound-cond neighbors neighborhood-len cond-tokens2 ...))]
		[(_ neighbors:expr neighborhood-len:expr 
        (~datum not) cond-tokens:expr ...)
			#'(not 
					(parse-compound-cond neighbors neighborhood-len cond-tokens ...) )]
    [(_ neighbors:expr neighborhood-len:expr other:expr ...)
      #' (parse-condition neighbors neighborhood-len other ...)]))

;; 
(define-syntax (parse-transition stx)
  (syntax-parse stx
    [(_ ((~datum _) (~datum ->) to-state:expr) _ condition:expr fallback:expr)
     #'(if condition to-state fallback)]
    [(_ (from-state (~datum ->) to-state) cur-state:expr condition:expr fallback:expr)
     #'(if (and (eq? cur-state from-state) condition) 
            to-state 
            fallback)]))

;; Syntax -> Listof Syntax
(define-for-syntax (deconstruct-transition stx)
  (syntax-parse stx
    [(from-state:expr (~datum ->) to-state:expr) (list stx)]
    [(from-state:expr (~datum ->) to-state:expr more:expr ...+)
     (append 
      (deconstruct-transition #'(to-state more ...))
      (list #'(from-state -> to-state)))]))

;; 
(define-syntax (parse-clauses stx)
  (syntax-parse stx
    [(_ neighbors:id neighborhood-len:expr state-type:id cur-state:expr [transition:expr condition-token:expr ...] 
               clauses ...)
     (define/syntax-parse 
       (current-simple-transition more-simple-transitions ...) 
       (deconstruct-transition #'transition))
     #'(parse-transition 
            current-simple-transition 
            cur-state 
            (parse-compound-cond neighbors neighborhood-len condition-token ...)
            (parse-clauses neighbors neighborhood-len state-type 
                         cur-state (more-simple-transitions condition-token ...) ... clauses ...)
            )]
            
    [(_ neighbors _ _ _)
     #'(lambda (state)
         (error (format "No valid transition from state ~a" state)))]))


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
           (parse-clauses neighbors (set-count neighborhood) state-type in-state clauses ...)))]))

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
