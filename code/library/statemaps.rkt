#lang typed/racket

(require "../types.rkt" "../utils.rkt"
    "colormaps.rkt"
    "topologies.rkt"
    "neighborhoods.rkt"
    racket/hash
    (for-syntax syntax/parse syntax/macro-testing))
(require/typed racket/hash 
    [hash-filter (All (C S) (-> (StateMap C S) (-> C S Boolean) (StateMap C S)))]
    [hash-filter-keys (All (K V) (-> (HashTable (U K Void) V) 
                                         (-> (U K Void) Boolean) 
                                         (Immutable-HashTable K V)))]) ; This is wrong and a hack because we really just don't care anymore
(module+ test (require typed/rackunit))

(define ALIVE-OR-DEAD-STATES : (Listof AliveOrDead) (list 'dead 'alive))

;; Creates an empty statemap
(: statemap-init : (All (C S) (-> (StateMap C S))))
(define statemap-init make-hash)

(: statemap-construct : (All (C S) (-> (Listof (Pair C S)) (StateMap C S))))
(define statemap-construct make-hash)


;; EFFECT: Sets a cell to have the given state in the provided statemap, potentially overwriting old an value
(: statemap-set! : (All (C S) (-> (StateMap C S) C S Void)))
(define statemap-set! hash-set!)

;; Copies a statemap
(: statemap-copy : (All (C S) (-> (StateMap C S) (StateMap C S))))
(define statemap-copy hash-copy)

(: statemap-remove-cell! : (All (C S) (-> (StateMap C S) C Void)))
(define statemap-remove-cell! hash-remove!)

;; Creates a copy of the statemap containing only values that pass the predicate
(: statemap-filter (All (C S) (-> (StateMap C S) (-> C S Boolean) (StateMap C S))))
(define statemap-filter hash-filter)

;; Modifies the states of a statemap using the given function applied on each cell-state pair
(: statemap-set-states! : (All (C S) (-> (StateMap C S) (-> C S S) Void)))
(define (statemap-set-states! statemap f)
    (statemap-for-each statemap (lambda ([c : C] [s : S]) (statemap-set! statemap c (f c s)))))

;; Applies the procedure for each cell-state pair
(: statemap-for-each : (All (C S) (-> (StateMap C S) (-> C S Void) Void)))
(define (statemap-for-each statemap f)
    (hash-for-each statemap f))

(: statemap-map : (All (C S C2 S2) (-> (StateMap C S) (-> C S (Values C2 S2)) (StateMap C2 S2))))
(define (statemap-map statemap f)
    (define res : (StateMap C2 S2) (statemap-init))
    (statemap-for-each 
        statemap
        (lambda ([c : C] [s : S]) 
            (let-values ([(new-c new-s) (f c s)])
                (statemap-set! res new-c new-s))))
    res)

(module+ test
    (check-equal?
    (statemap-map 
        (statemap-construct (list (list 0 #t) (list 1 #f))) 
        (lambda (c s) 
            (values 
                (if s (add1 c) (sub1 s))
                (not s))))
    (statemap-construct (list (list 1 #f) (list 0 #t))))
)

;; Creates a new statemap that contains the provided statemaps
(: statemap-merge (All (C S) (-> (StateMap C S) (StateMap C S) (StateMap C S))))
(define (statemap-merge s1 s2)
    (define res (statemap-copy s1))
    (define f (lambda ([c : C] [s : S]) (statemap-set! res c s)))
    (statemap-for-each s2 f)
    res)


;; Utility constructor for a simple rectangular grids. Accepts a list of rows going from top to bottom, where each row is a list of states going from left to right. Ragged grids are ok. 
(: make-statemap-2d : (All (S) (-> (Listof S) * (StateMap Posn S))))
(define (make-statemap-2d . rows)
    (define ret : (StateMap Posn S) (statemap-init))
    (: make-row-of-states : (Pairof Nonnegative-Integer (Listof S)) -> Void)
    (define (make-row-of-states row-with-index)
        (match-define (cons row-index row) row-with-index)
        (: handle-cell : (Pairof Nonnegative-Integer S) -> (Pairof Posn S))
        (define (handle-cell cell-with-index)
            (match-define (cons col-index state) cell-with-index)
            (cons (Posn col-index row-index) state))
        (for ([v (map handle-cell (enumerate row))]) 
            (statemap-set! ret (car v) (cdr v))))
    (for ([v (enumerate rows)])
        (make-row-of-states v))
    ret)
(module+ test
    (check-equal? (make-statemap-2d '(0-0 1-0) '(0-1) '(0-2 1-2 2-2))
        (hash 
            (Posn 0 0) '0-0 (Posn 1 0) '1-0
            (Posn 0 1) '0-1
            (Posn 0 2) '0-2 (Posn 1 2) '1-2 (Posn 2 2) '2-2))
) 

;; Cells enclosed by a rectangle whose diagonal runs between the two given points
(: cells-in-region : Posn Posn -> [Setof Posn])
(define (cells-in-region corner1 corner2)
    (match-define (list x1 x2) (sort (list (posn-x corner1) (posn-x corner2)) <))
    (match-define (list y1 y2) (sort (list (posn-y corner1) (posn-y corner2)) <))
    (for*/set : [Setof Posn] ([x : Integer (in-range x1 (add1 x2))] [y : Integer (in-range y1 (add1 y2))]) (Posn x y))) ; in-inclusive-range does not have proper type annotations
(module+ test
    (define EXAMPLE-SET-1 (set (Posn 0 0) (Posn 0 1) (Posn 0 2) (Posn 1 0) (Posn 1 1) (Posn 1 2) (Posn 2 0) (Posn 2 1) (Posn 2 2)))
    (check-equal? (cells-in-region (Posn 0 0) (Posn 2 2)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 2 2) (Posn 0 0)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 0 2) (Posn 2 0)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 2 0) (Posn 0 2)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 3 2) (Posn 3 3)) (set (Posn 3 2) (Posn 3 3))))

;; Reshapes an interleaved sequence of two types into a list of pairs
(: split-interleaved (-> (Rec x (U Null (List* S Nonnegative-Integer x)))
                          (Values (Listof S) (Listof Nonnegative-Integer))))
(define (split-interleaved items)
    (let loop ([items items]
            [states : (Listof S) '()]
            [biases : (Listof Nonnegative-Integer) '()])
    (if (pair? items)
        (loop (cddr items)
                (cons (first items) states)
                (cons (second items) biases))
        (values (reverse states) (reverse biases)))))

;; Reshapes an interleaved sequence of two types into a list of pairs
(: reshape-interleaved : (All (A B) (-> (Rec x (U Null (List* A B x))) [Listof [Pairof A B]])))
(define (reshape-interleaved interleaved)
    (cond 
        [(empty?) '()]
        [(empty? rest) (error 'reshape-interleaved "An even number of items should have been provided")]
        [else (cons 
            (cons (car interleaved) (cdar interleaved)) 
            (reshape-interleaved (cddr interleaved)))]))
(module+ test 
    (check-equal? (reshape-interleaved (list "hi" 3)) (list (cons "hi" 3)))
    (check-equal? (reshape-interleaved (list "hi" 4 "bye" -3)) (list (cons "hi" 4) (cons "bye" -3))))

;; Generates a state-threshold list from a list of states and biases
(: generate-state-threshold : (All (S) (-> [Listof [Pairof S Nonnegative-Integer]] [Listof [Pairof S Nonnegative-Integer]])))
(define (generate-state-threshold states+biases)
    (unless (= (length states) (length (remove-duplicates states))) 
        (error 'random-chooser-biased (format "Duplicate state specified: ~a" (check-duplicates states))))
    (: sum-so-far : (-> (Listof (Pair S Nonnegative-Integer) Nonnegative-Integer  Nonnegative-Integer)))
    (define (sum-so-far lst state)
        (cond
            [(empty? lst) (raise "No matching list item")]
            [(equal? state (car (first lst))) (cdr (first lst))]
            [else (+ (cdr (first lst)) (sum-so-far (rest lst) state))]))
            
    (map (lambda ([pair : (Pair S Nonnegative-Integer)]) 
        (cons (first pair) (sum-so-far states+biases (first pair))))
            states+biases))
(module+ test
    (define EXAMPLE-STATE-THRESHOLD 
        (generate-state-threshold 
            (list (cons 'a 5) (cons 'b 7) (cons 'c 3))) 
            (list (cons 'a 0) (cons 'b 5) (cons 'c 12)))
    (check-equal? EXAMPLE-STATE-THRESHOLD 
        (list (cons 'a 0) (cons 'b 5) (cons 'c 12))))

;; Returns the state corresponding to the highest threshold in the state-threshold list less than the given value
(: apply-state-threshold : (All (S) (-> Nonnegative-Integer (ListOf (Pair S Nonnegative-Integer) S))))
(define (apply-state-threshold state-thresholds value)
  (car (sort 
        (lambda ([p1 : (Pairof S Nonnegative-Integer)] 
                [p2 : (Pairof S Nonnegative-Integer)]) 
            (< (cdr p1) (cdr p2))) 
        (filter (lambda (t) (> (cdr t) value)) state-thresholds))))
(module+ test
    (check-equal? (apply-state-threshold EXAMPLE-STATE-THRESHOLD 0) 'a)
    (check-equal? (apply-state-threshold EXAMPLE-STATE-THRESHOLD 4) 'a)
    (check-equal? (apply-state-threshold EXAMPLE-STATE-THRESHOLD 5) 'b)
    (check-equal? (apply-state-threshold EXAMPLE-STATE-THRESHOLD 11) 'b)
    (check-equal? (apply-state-threshold EXAMPLE-STATE-THRESHOLD 12) 'c)
    (check-equal? (apply-state-threshold EXAMPLE-STATE-THRESHOLD 14) 'c))

;; Randomly chooses a state from the provided list, which can optionally be provided with weights to bias the output
(: random-chooser-biased : (All (S) (->* () #:rest-star (S Nonnegative-Integer) (-> S))))
(define (random-chooser-biased . states+biases)
    (: split-interleaved (-> (Rec x (U Null (List* S Nonnegative-Integer x)))
                          (Values (Listof S) (Listof Nonnegative-Integer))))
    (define (split-interleaved items)
        (let loop ([items items]
                [states : (Listof S) '()]
                [biases : (Listof Nonnegative-Integer) '()])
        (if (pair? items)
            (loop (cddr items)
                    (cons (first items) states)
                    (cons (second items) biases))
            (values (reverse states) (reverse biases)))))
    (define-values (states biases) (split-interleaved states+biases))

    (unless (= (length states) (length (remove-duplicates states))) 
        (error 'random-chooser-biased (format "Duplicate state specified: ~a" (check-duplicates states)))
    
    (: sum-so-far : (-> (Listof (Pair S Nonnegative-Integer) Nonnegative-Integer  Nonnegative-Integer)))
    (define (sum-so-far list state)
        (cond
            [(empty? list) (raise "No matching list item")]
            [(equal? state (car (first list))) 0]
            [else (+ (cdr (first list)) (sum-so-far (rest list) state)]))
            
    (define state-thresholds 
        (map (lambda ([pair : (Pair S Nonnegative-Integer)]) 
            (values (first pair) (sum-so-far states+biases (first pair))))
             states+biases))
             
    (lambda () (let ([r (random total)])
        (car (first (filter (lambda (t) (> (cdr t) r)) state-thresholds)))))))

#;(: random-chooser-biased (All (S) (->* () #:rest-star (S Nonnegative-Exact-Rational) (-> S))))
#;(define (random-chooser-biased . states+biases)
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
    (define r (random total-bias)))
    (let loop ([states states]
               [biases biases]
               [sum : Nonnegative-Exact-Rational 0])
      (cond
        [(null? states) (error 'random-chooser-biased "Empty state list")]
        [else
         (define new-sum (+ sum (first biases)))
         (if (< r new-sum)
             (first states)
             (loop (rest states) (rest biases) new-sum))])))
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
(: rect-solid : (All (S) (Positive-Integer Positive-Integer S -> (StateMap Posn S))))
(define (rect-solid width height state)
    (rect-random width height (lambda () state)))

;; Creates a statemap over a rectangular region with cells initialized using the given thunk
(: rect-random : (All (S) Positive-Integer Positive-Integer (-> S) -> (StateMap Posn S)))
(define (rect-random width height rand-state-generator)
    (rect-custom width height (lambda (_) (rand-state-generator))))

;; Creates a statemap over a rectangular region with cells initialized using the given function
(: rect-custom : (All (S) Positive-Integer Positive-Integer (-> P S) -> (StateMap Posn S)))
(define (rect-custom width height state-fn)
    (define x : [Listof (Pairof Posn S)] (for/list : [Listof (Pairof Posn S)]
        ([cell : Posn (cells-in-region (Posn 0 0) (Posn width height))]) 
        (cons cell (state-fn cell))))
    (statemap-construct x))

;; Accepts arguments alternating between an absolute position and then a statemap to placed with its lower left corner at that absolute position.
;; Similar functionality to overlay/xy in htdp2/image
(: overlay/statemaps (All (C O S) (->* ([Topology C O]) #:rest-star (O (StateMap C S)) (StateMap C S))))
(define (overlay/statemaps topology . absolute-posn-shape) 
    (cond
        [(empty? absolute-posn-shape) (make-hash)]
        [(empty? (cdr absolute-posn-shape)) (raise-syntax-error "Must provide rest arguments alternating between offset and statemap. Provided was of odd length.")]
        [else (let* 
            ([offset (car absolute-posn-shape)]
            [shape (car (cdr absolute-posn-shape))]
            [translated-shape (statemap-filter (statemap-map shape (lambda ([k : C] [v : S]) (values (topology k offset) v))) (compose not void?))])
            (statemap-merge translated-shape (apply overlay/statemaps topology (cddr absolute-posn-shape))))]))
(module+ test
    (define s1 
    (make-statemap-2d 
    (list 'dead 'dead 'dead)
    (list 'dead 'alive 'dead)
    (list 'dead 'dead 'dead)))
    (define s2 (make-statemap-2d (list 'alive)))
    (define s3 
    (make-statemap-2d 
    (list 'dead 'dead 'dead)
    (list 'dead 'alive 'dead)
    (list 'dead 'alive 'dead)))
    (check-equal? (overlay/statemaps (make-finite-cartesian-topology 5 5) (Posn 0 0) s1 (Posn 1 2) s2)))

(provide make-statemap-2d ALIVE-OR-DEAD-STATES rect-custom rect-random rect-solid random-chooser random-chooser-biased)






;;; Graveyard


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