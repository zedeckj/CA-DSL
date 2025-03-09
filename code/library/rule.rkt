#lang typed/racket
(require "../types.rkt" "../utils.rkt" "topologies.rkt")

(: get-neighbors : (All (C O S) (C (StateMap C S) (Topology O S) (Neighborhood O) -> (Listof S))))
(define (get-neighbors cell state-map topology neighbors)
    (map 
        (lambda ([cell : C]) (hash-ref state-map cell))
        (foldr 
            (lambda ([cur : (Union C Void)] [acc : (Listof C)]) 
                (if (void? cur) acc (cons cur acc))) 
            (list) 
            (set-map 
                (lambda ([offset : O]) (topology cell offset))
                neighbors))))

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

( : mapper : (All (C S) (StateMap C S) (C S -> C) -> (StateMap C S)))
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

#lang typed/racket
(require (for-syntax syntax/parse))

(define-syntax (parse-branches stx)
  (syntax-parse stx
    [(_ [(in-state:expr (~datum ->) out-state:expr) cond:expr] ...) #'(if (and ((parse-cond cond) 
    [(_ [default body:expr]) #'body]))

(define-syntax (minirule stx)
    (syntax-parse stx
        [(_ name:id branches ...)
        #'(define (conway state-map topology)





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