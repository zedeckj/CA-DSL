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
    (check-equal? (get-neighbors (Posn 1 1) STATEMAP-3x3-LIVE-CROSS cartesian-topology(moore-neighborhood)) 4))

;; Checks if the number of neighbors in the specified state is one of the provided counts. 
(: has-neighbors-in-state? : (All (S) (-> S (Listof S) (Listof Nonnegative-Integer) Boolean)))
(define (has-neighbors-in-state? state neighbors counts)
    (let ([found-count 
            (count  
                (lambda ([neighbor : S]) (equal? neighbor state)) 
                neighbors)])
        (ormap (lambda ([c : Nonnegative-Integer]) (= c found-count)) counts)))

(: id-rule : (All (C O S) (Rule C O S)))
;; An identity rule, which produces the same StateMap it recieves
(define (id-rule state-map topology) 
    state-map)

(: alternating-rule : (All (C O) (Rule C O AliveOrDead)))
(define (alternating-rule state-map _)
    (mapper state-map
        (lambda ([_ : C]
                 [state : AliveOrDead])
            (if (equal? state 'alive) 'dead 'alive))))


(: mapper : (All (C S) (StateMap C S) (C S -> S) -> (StateMap C S)))
;; Applies a function which takes a cell and state to a copy of a given StateMap
(define (mapper state-map rule-body)
    (hash-map/copy 
        state-map 
        (lambda ([cell : C] [in-state : S]) 
            (values cell (rule-body cell in-state)))))


;; Rule for Conway's game of life using states 'alive and 'dead
(: conway-rule : (Rule Posn Posn AliveOrDead))
(define (conway-rule state-map topology)
    (mapper state-map
        (lambda ([cell : Posn]
                [in-state : AliveOrDead])
            (let ([neighbors (get-neighbors cell state-map topology (moore-neighborhood))])
                (match in-state
                    ['alive (if (has-neighbors-in-state? 'alive neighbors '(2 3)) 'alive 'dead)]
                    ['dead (if (has-neighbors-in-state? 'alive neighbors '(3))'alive 'dead)])))))

(provide conway-rule alternating-rule id-rule)

#;(minirule 
    #:neighborhood moore
    [('alive -> 'alive) ((2 3) in 'alive)]
    [('dead -> 'alive) ((3) in 'alive)]
    [default 'dead])

; (define-type (RuleTransition S) (Pairof S S))
; (define-type (RuleCondition C S) (C S (Neighborhood S) -> Boolean))
; (define-type (RuleBranch C S) (Pairof (RuleTransition S) (RuleCondition S S)))

; (: rt:-> : (All (S) (S S -> (RuleTransition S))))
; (define (rt:-> in-state out-state)
;     (cons in-state out-state))


; (: rt:in : (All (S) (S (Listof Nonnegative-Integer) -> (RuleCondition S Any))))
; (define (rt:in target-state allowed-counts)
;     (lambda (_ _ [neighbors : (Neighborhood S)])
;     (let ([found-count 
;             (count  
;                 (lambda ([neighbor : S]) (equal? neighbor target-stat)) 
;                 neighbors)])
;         (ormap (lambda ([c : Nonnegative-Integer]) (= c found-count)) allowed-counts))))

; (: rt:rule : )

; (require (for-syntax syntax/parse))

; (define-syntax (parse-branches stx)
;   (syntax-parse stx
;     [(_ [(in-state:expr (~datum ->) out-state:expr) cond:expr] ...) #'(if (and ((parse-cond cond) 
;     [(_ [default body:expr]) #'body]))

; (define-syntax (minirule stx)
;     (syntax-parse stx
;         [(_ name:id branches ...)
;         #'(define (conway state-map topology)





#|

#lang racket
(require (for-syntax racket/syntax syntax/parse))


  (define-syntax (define-a stx)
    (syntax-parse stx
      [(_define-a expr)
       (define a (format-id stx "~a" 'a))
       (with-syntax ([a a])
         #'(define a expr))]))
  
(define-a 42)


|#