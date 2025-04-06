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

(define-syntax (parse-count stx)
  (syntax-parse stx
    [(_ neighbors:expr neighborhood-len count:expr)
    ;; Should be moved to static check
     #'(if
        (> count neighborhood-len)
        (raise-syntax-error (format "Count must not exceed size of neighborhood (~a)" neighborhood-len) count)
        count)]
    ))

;; Processes the neighbors
(define-syntax (parse-counts stx)
  (syntax-parse stx
    [(_ neighbors:expr _ (~datum all))
     #'(length neighbors)]
    [(_ neighbors:expr neighborhood-len (count:expr ... ))
     #'(list (parse-counts neighbors neigborhood-len count) ...)]))


(define-syntax (parse-condition stx)
  (syntax-parse stx
    [(_ neighbors:expr neighborhood-len:expr (count:expr ...+) (~datum in) state:expr)
     #'(let
           ([count-list : (Listof Nonnegative-Integer) (list count ...)])
         (has-neighbors-in-state? state neighbors count-list))]
    [(_ neighbors:expr neighborhood-len:expr count:expr (~datum in) state:expr)
     #'(let ([count-var : Nonnegative-Integer count])
         (has-neighbors-in-state? state neighbors (list count-var)))]
    [(_ neighbors:expr neighborhood-len) #'#t]))


#;(define-syntax (parse-compound-cond stx)
	(syntax-parse stx
		[(_ neighbors:expr neighborhood-len:expr cond-tokens1:expr ... (~datum and) cond-tokens2:expr ...)
			#'(and 
					(parse-condition neighbors neighborhood-len cond-tokens1 ...) 
					(parse-condition neighbors neighborhood-len cond-tokens2 ...))]))

(define-syntax (parse-transition stx)
  (syntax-parse stx
    [(_ ((~datum _) (~datum ->) out-state:expr))
     #'(lambda (cur-state cond fallback) 
         (if (cond) out-state (fallback cur-state)))]
    [(_ (in-state (~datum ->) out-state))
     #'(lambda (cur-state cond fallback)
         (if (and (eq? cur-state in-state) (cond)) 
            out-state 
            (fallback cur-state)))]))

;;(transition in-state other-condition fallback-callback)
;;(lambda (in-state) (transition (in-state other-conditions fallback-callback))))

;; Syntax -> Listof Syntax
(define-for-syntax (deconstruct-transition stx)
  (syntax-parse stx
    [(in-state:expr (~datum ->) out-state:expr) (list stx)]
    [(in-state:expr (~datum ->) out-state:expr more:expr ...+)
     (append 
      (deconstruct-transition #'(out-state more ...))
      (list #'(in-state -> out-state)))]))

(define-syntax (parse-clauses stx)
  (syntax-parse stx
    [(_ neighbors:id neighborhood-len:expr state-type:id [transition:expr condition-token:expr ...] 
              clauses ...)
     (define/syntax-parse 
       (current-simple-transition more-simple-transitions ...) 
       (deconstruct-transition #'transition))
     #'(lambda ([in-state : state-type])
         ((parse-transition current-simple-transition)
          in-state
          (lambda () (parse-condition neighbors neighborhood-len condition-token ...))
          ; is a thunk to delay execution for performance gains and short-circuiting
          (parse-clauses neighbors neighborhood-len state-type 
                         (more-simple-transitions condition-token ...) ... clauses ...)))]
            
    [(_ neighbors _ _)
     #'(lambda (state)
         (error (format "No valid transition from state ~a" state)))]))


;; Main macro for declaring a Rule for a cellular automata
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
           ((parse-clauses neighbors (set-count neighborhood) state-type clauses ...) in-state)))]))
; Pass down the length of the full neighborhood to enable runtime checks of conditions which specify a number of cells that doesn't make sense


;; Shorthand macro for the common usecase of making a Rule with Posn cell and offset types, and a neighborhood consisting of the Moore neighborhood with a radius of 1
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

;; Shorthand for making a rule with alive and dead states with a default state of dead and uses a moore neighborhood with cells and offsets represented as Posn
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
