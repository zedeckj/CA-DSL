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

(define-type WireWorldState (U 'head 'tail 'conductor 'empty))
(define wireworld ;; https://conwaylife.com/wiki/OCA:WireWorld
    (rule
        #:cell-type Posn
        #:offset-type Posn
        #:state-type WireWorldState
        #:neighborhood (moore-neighborhood)
        [('head -> 'tail -> 'conductor)]
        [('conductor -> 'head) (1 2) in 'head]
        [('conductor -> 'conductor)]
        [(_ -> 'empty)]))

           
(define world : (2DWorld WireWorldState) (random-world 50 50 (list 'head 'tail 'conductor 'empty)))
(define renderer : (2DRenderer WireWorldState) (make-2d-renderer (make-default-colormap 'head 'tail 'conductor 'empty)))
(run world wireworld renderer)

;; Cells enclosed by a rectangle whose diagonal runs between the two given points
(: cells-in-region : Posn Posn -> [Setof Posn])
(define (cells-in-region corner1 corner2)
    (match-define (list x1 x2) (sort (list (posn-x corner1) (posn-x corner2)) <))
    (match-define (list y1 y2) (sort (list (posn-y corner1) (posn-y corner2)) <))
    (for*/set : [Setof Posn] ([x : Integer (in-range x1 x2)] [y : Integer (in-range y1 y2)]) (Posn x y))) ; in-inclusive-range does not have proper type annotations
(module+ test
    (define EXAMPLE-SET-1 (set (Posn 0 0) (Posn 0 1) (Posn 0 2) (Posn 1 0) (Posn 1 1) (Posn 1 2) (Posn 2 0) (Posn 2 1) (Posn 2 2)))
    (check-equal? (cells-in-region (Posn 0 0) (Posn 2 2)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 2 2) (Posn 0 0)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 0 2) (Posn 2 0)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 2 0) (Posn 0 2)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 3 2) (Posn 3 3)) (set (Posn 3 2) (Posn 3 3))))

(: random-chooser-biased (All (S) (->* () #:rest-star (S Nonnegative-Exact-Rational) (-> S))))
(define (random-chooser-biased . states+biases)
  (: split-interleaved (-> (Rec x (U Null (List* S Nonnegative-Exact-Rational x)))
                          (Values (Listof S) (Listof Nonnegative-Exact-Rational))))
  (define (split-interleaved items)
    (let loop ([items items]
               [states : (Listof S) '()]
               [biases : (Listof Nonnegative-Exact-Rational) '()])
      (if (pair? items)
          (loop (cddr items)
                (cons (first items) states)
                (cons (second items) biases))
          (values (reverse states) (reverse biases)))))
  
  (define-values (states biases) (split-interleaved states+biases))
  (define total-bias (apply + biases))
  (lambda ()
    (define r (* total-bias (random)))
    (let loop ([states states]
               [biases biases]
               [sum : Nonnegative-Exact-Rational 0])
      (cond
        [(null? states) (error 'random-chooser-biased "Empty state list")]
        [else
         (define new-sum (+ sum (first biases)))
         (if (< r new-sum)
             (first states)
             (loop (rest states) (rest biases) new-sum))]))))

;; Randomly chooses a state from the provided list, which can optionally be provided with weights to bias the output
#;(: random-chooser-biased : (All (S) (->* () #:rest-star (S Nonnegative-Exact-Rational) (-> S))))
#;(define (random-chooser-biased . states+biases) 
    (let* (
    [states : [Listof S] (map first states+biases)]
    [biases : [Listof Nonnegative-Exact-Rational] (map second states+biases)]
    [lowest-non-zero (first (sort (filter positive? biases)))]
    [scale-factor (/ 1 lowest-non-zero)]
    [biases : [Listof Integer] (map (lambda (v) (* scale-factor v)) biases)]
    [total : Integer (foldl + biases)]
    [state-thresholds : [Listof [Pairof S Integer]] 
        (second (list (for/fold ([acc-sum : Integer 0] [acc-list : [Listof [Pairof S Integer]] '()])
            ([cur-state states] [cur-bias biases])
            (values (+ acc-sum cur-bias) (append acc-list (list (cons cur-state (+ acc-sum cur-bias))))))))]) 
    (lambda () (let ([r (random total)])
        (car (first (filter (lambda (t) (> (cdr t) r)) state-thresholds)))))))
(module+ test
    (check-equal? ((random-chooser-biased "hello" 3)) "hello")
    (check-equal? ((random-chooser-biased "hello" 0 "bye" 10 "no" 0 "ok" 0)) "bye")
    (check-equal? ((random-chooser-biased "hello" #e0.03 "hello" 4)) "hello"))

;; Creates a thunk to randomly choose an item from the list
(: random-chooser : (All (T) ([Listof T] -> (-> T))))
(define (random-chooser lst)
    (define-type Interleave (Rec x (U Null (List* T One x))))
    (: interleave-bias : [Listof T] -> Interleave)
    (define (interleave-bias l)
      (let loop ([items : (Listof T) l]
                 [result : Interleave '()])
        (if (null? items)
            result
            (loop (rest items)
                  (ann (cons (first items) (ann (cons 1 (ann result Interleave)) (Pairof One Interleave))) Interleave)))))
    (apply random-chooser-biased (interleave-bias lst)))
(module+ test
    (check-equal? ((random-chooser (list 5 5))) 5)
    (check-equal? ((random-chooser (list "hello"))) "hello"))

;; Creates a statemap of a rectangle with states randomly chosen from the provided list
(: rect-solid : (All (S) (Posn Posn S -> (StateMap Posn S))))
(define (rect-solid corner1 corner2 state)
    (rect-random corner1 corner2 (lambda () state)))

;; Creates a statemap over a rectangular region with cells initialized to random states
(: rect-random : (All (S) Posn Posn (-> S) -> (StateMap Posn S)))
(define (rect-random corner1 corner2 rand-state-generator)
    (rect-custom corner1 corner2 (lambda (_) (rand-state-generator))))

;; Creates a statemap over a rectangular region with cells initialized to random states
(: rect-custom : (All (S) Posn Posn (-> Posn S) -> (StateMap Posn S)))
(define (rect-custom corner1 corner2 state-fn)
    (define x : [Listof (Pairof Posn S)] (for/list : [Listof (Pairof Posn S)]
        ([cell : Posn (cells-in-region corner1 corner2)]) 
        (cons cell (state-fn cell))))
    (make-hash x))

#|
StateMap/World stuff:
    random-rect 
    rect 
    2DWorld, 2DRender types

    (cartesian-world (random-rect ...))

    make-state-map

    wire function


(define wireworld ;; https://conwaylife.com/wiki/OCA:WireWorld
    (moore-rule 
        #:state-type WireWorldState
        [('head -> 'tail -> 'conductor)]
        [('conductor -> 'conductor)]
        [('conductor -> 'head) (1 2) in 'head]
        [default 'empty]))



|#

#|
(define (random-choice stx)
    (syntax-parse stx
        [((~datum :) state-type:id [state:expr bias:expr] ...+) #'(let* (
                [states (list (ann state S) ...)]
                [biases (list (ann biases Nonnegative-Exact-Rational))]
                [lowest-non-zero (first (sort (filter positive? biases)))]
                [scale-factor (/ 1 lowest-non-zero)]
                [biases (map (lambda (v) (* scale-factor v)) biases)]
                [total (+ bias ...)]
                [r (random total)]
                [state-thresholds (second (list (for/fold ([acc-sum 0] [acc-list '()])
                    ([cur-state (list state ...)] [cur-bias biases])
                    (values (+ acc-sum cur-bias) (append acc-list (list (cons cur-state (+ acc-sum cur-bias))))))))]
                (car (first (filter (lambda (t) (> (cdr t) r)) state-thresholds)))))]
        [((~datum :) state-type:id state:expr ...+) #'(random-choice state-type:id [state 1] ...)]
        [(state:expr ...+) #'(random-choice : Any state ...)]
        [([state:expr bias:expr] ...+) #'(random-choice : Any [state bias] ...)]))

#|
::= (random-choice <optional-type-ann> <states>)
<optional-type-ann> ::= | : <id:Type>
<states> ::= <states-without-biases> | <states-with-biases> 
<states-without-biases> ::= <expr:S> | <expr:S> <states-without-biases>
<states-with-biases> ::= <state-with-bias> | <state-with-bias> <states-with-biases> 
<state-with-bias> ::= [<expr:S> <expr:Nonnegative-Exact-Rational>]
|#
;; Randomly chooses a state from the provided list, which can optionally be provided with weights to bias the output
(define-syntax (random-choice stx)
    (syntax-parse stx
        [((~datum :) state-type:id [state:expr bias:expr] ...+) #'(let* (
                [states (list (ann state S) ...)]
                [biases (list (ann biases Nonnegative-Exact-Rational))]
                [lowest-non-zero (first (sort (filter positive? biases)))]
                [scale-factor (/ 1 lowest-non-zero)]
                [biases (map (lambda (v) (* scale-factor v)) biases)]
                [total (+ bias ...)]
                [r (random total)]
                [state-thresholds (second (list (for/fold ([acc-sum 0] [acc-list '()])
                    ([cur-state (list state ...)] [cur-bias biases])
                    (values (+ acc-sum cur-bias) (append acc-list (list (cons cur-state (+ acc-sum cur-bias))))))))]
                (car (first (filter (lambda (t) (> (cdr t) r)) state-thresholds)))))]
        [((~datum :) state-type:id state:expr ...+) #'(random-choice state-type:id [state 1] ...)]
        [(state:expr ...+) #'(random-choice : Any state ...)]
        [([state:expr bias:expr] ...+) #'(random-choice : Any [state bias] ...)]))
        |#


        #|
::= (rect <optional-type-ann> <expr:Posn> <expr:Posn> <states>)
<optional-type-ann> ::= | : <id:Type>
<states> ::= <states-without-biases> | <states-with-biases> 
<states-without-biases> ::= <expr:S> | <expr:S> <states-without-biases>
<states-with-biases> ::= <state-with-bias> | <state-with-bias> <states-with-biases> 
<state-with-bias> ::= [<expr:S> <expr:Nonnegative-Exact-Rational>]
|#