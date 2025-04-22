#lang typed/racket

(require "types.rkt" "utils.rkt"
    "colormaps.rkt"
    "topologies.rkt"
    racket/hash
    (for-syntax syntax/parse syntax/macro-testing))
(require/typed racket/hash 
    [hash-filter (All (C S) (-> (StateMap C S) (-> C S Boolean) (StateMap C S)))]
    [hash-filter-keys (All (K V) (-> (HashTable (U K Void) V) 
                                         (-> (U K Void) Boolean) 
                                         (Immutable-HashTable K V)))]) ; This is wrong and a hack because we really just don't care anymore
(require/typed racket/random
    [random-ref (All (S) (-> (Listof S) S))])

(module+ test (require typed/rackunit))

;; A union types of different descriptions of Direction used for drawing paths 
(define-type Direction (U 'left 'right 'down 'up))

(define direction-offset
    (hash 'up (Posn 0 -1)
        'down (Posn 0 1)
        'left (Posn -1 0)
        'right (Posn 1 0)))

(: direction->offset : (-> Direction Posn))
(define (direction->offset d) (hash-ref direction-offset d))

;; Creates an empty statemap
(: statemap-init : (All (C S) (-> (StateMap C S))))
(define statemap-init make-hash)

;; Creates a statemap with the given list of pairs of Cells and the States they are in
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

;; Creates a new StateMap from the result of applying the given procedure `f` to each Cell and State in 
;; the provided statemap. If `f` returns void, the value is not used
(: statemap-map : (All (C S C2 S2) (-> (StateMap C S) (-> C S (U Void (Pairof C2 S2))) (StateMap C2 S2))))
(define (statemap-map statemap f)
    (define res : (StateMap C2 S2) (statemap-init))
    (statemap-for-each 
        statemap
        (lambda ([cell : C] [state : S]) 
            (let ([proc-res (f cell state)])
                (unless (void? proc-res)
                    (statemap-set! res (car proc-res) (cdr proc-res))))))
    res)
(module+ test
    (check-equal?
    (statemap-map 
        (statemap-construct (ann (list (cons 0 #t) (cons 1 #f)) (Listof (Pairof Integer Boolean)))) 
        (lambda ([c : Integer] [s : Boolean]) 
            (cons 
                (if s (add1 c) (sub1 c))
                (not s))))
    (statemap-construct (list (cons 1 #f) (cons 0 #t)))))

;; Creates a new statemap that contains the provided statemaps
(: statemap-merge (All (C S) (-> (StateMap C S) (StateMap C S) (StateMap C S))))
(define (statemap-merge s1 s2)
    (define res (statemap-copy s1))
    (define f (lambda ([c : C] [s : S]) (statemap-set! res c s)))
    (statemap-for-each s2 f)
    res)

;; Cells enclosed by a rectangle whose diagonal runs between the two given points
(: cells-in-region : Posn Posn -> [Setof Posn])
(define (cells-in-region corner1 corner2)
    (match-define (list x1 x2) (sort (list (posn-x corner1) (posn-x corner2)) <))
    (match-define (list y1 y2) (sort (list (posn-y corner1) (posn-y corner2)) <))
    (for*/set : [Setof Posn] 
        ([x : Integer (in-range x1 (add1 x2))] 
         [y : Integer (in-range y1 (add1 y2))]) 
         (Posn x y))) ; in-inclusive-range does not have proper type annotations

(module+ test
    (define EXAMPLE-SET-1 : (Setof Posn)
        (set (Posn 0 0) (Posn 0 1) (Posn 0 2) (Posn 1 0) 
             (Posn 1 1) (Posn 1 2) (Posn 2 0) (Posn 2 1) (Posn 2 2)))
    (check-equal? (cells-in-region (Posn 0 0) (Posn 2 2)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 2 2) (Posn 0 0)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 0 2) (Posn 2 0)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 2 0) (Posn 0 2)) EXAMPLE-SET-1)
    (check-equal? (cells-in-region (Posn 3 2) (Posn 3 3)) (set (Posn 3 2) (Posn 3 3))))


;; A much more simple implementation of creating a thunk to choose from a list of values with biases
(: biased-random-select : (All (S) (-> (Listof (Pairof S Nonnegative-Integer)) (-> S))))
(define (biased-random-select weighted-sequence)
    
    (define gcd-val : Nonnegative-Integer 
        (apply gcd (map (inst cdr S Nonnegative-Integer) weighted-sequence)))

    ;; Transforms a list of values with weights to a list of values with proportional repetitions
    (: make-selection-seq : (-> (Listof (Pairof S Nonnegative-Integer)) (Listof S)))
    (define (make-selection-seq w-seq)
        (cond
            [(empty? w-seq) '()]
            [else (append 
                        (build-list 
                            (floor (/ (cdr (car w-seq)) gcd-val)) 
                            (lambda (_) (car (car w-seq)))) 
                        (make-selection-seq (rest w-seq)))]))
                
    (let ([select-seq (make-selection-seq weighted-sequence)])
        (lambda () (random-ref (make-selection-seq weighted-sequence)))))

;; Creates a statemap of a rectangle composed othe given state
(: rect-solid : (All (S) (Integer Integer S -> (StateMap Posn S))))
(define (rect-solid width height state)
    (rect-from width height (lambda () : S state)))

;; Creates a statemap over a rectangular region with cells initialized using the given thunk
(: rect-from : (All (S) (Integer Integer (-> S) -> (StateMap Posn S))))
(define (rect-from width height state-generator)
    (rect-custom width height (lambda (_) : S (state-generator))))

;; Creates a statemap over a rectangular region with cells initialized using the given function
(: rect-custom : (All (S) Integer Integer (-> Posn S) -> (StateMap Posn S)))
(define (rect-custom width height state-fn)
    (ann (statemap-construct
        (for/list : [Listof (Pairof Posn S)]
            ([cell : Posn (cells-in-region (Posn 0 0) (Posn width height))]) 
            (cons cell (ann (state-fn cell) S)))) (StateMap Posn S)))

;; Accepts arguments alternating between an absolute position and then a statemap to be placed with its lower left corner at that absolute position.
;; Similar functionality to overlay/xy in htdp2/image
(: overlay/statemaps (All (C O S) (->* ([Topology C O]) #:rest-star (O (StateMap C S)) (StateMap C S))))
(define (overlay/statemaps topology . absolute-posn-shape) 
    (cond
        [(empty? absolute-posn-shape) (make-hash)]
        [(empty? (cdr absolute-posn-shape)) 
            (raise-syntax-error 
                "Must provide rest arguments alternating between "
                "offset and statemap. Provided was of odd length.")]
        [else 
            (let* 
                ([offset (car absolute-posn-shape)]
                [shape (car (cdr absolute-posn-shape))]
                [translated-shape 
                    (statemap-map 
                        shape 
                        (lambda ([cell : C] [state : S]) 
                            (let ([new-cell (topology cell offset)])
                                (unless (void? new-cell)
                                    (cons new-cell state)))))])
                (statemap-merge translated-shape (apply overlay/statemaps topology (cddr absolute-posn-shape))))]))

;; Creates a statemap with the given cells initialized to the given state
(: fill-cells : (All (C S) (-> S (Setof C) (StateMap C S))))
(define (fill-cells state cells)
    (statemap-construct (set-map (lambda ([cell : C]) (cons cell state)) cells)))

;; Internal function to create a statemap of the shape of a width 1 path through a cartesian grid 
(: path-internal : (All (S) (-> (Topology Posn Posn) 
                                Posn 
                                (Listof (List S Nonnegative-Integer Direction)) 
                                (StateMap Posn S))))
(define (path-internal topology start segs)
    (: path-helper : (-> (Listof (List S Nonnegative-Integer Direction)) 
                        (StateMap Posn S)))
    (define (path-helper segs)
        (cond
            [(empty? segs) (statemap-init)]
            [(let* ([cur-seg (first segs)]
                    [state (first cur-seg)]
                    [magn (second cur-seg)]
                    [dir (third cur-seg)]
                    [offset (posn-scale (sub1 magn) (direction->offset dir))]
                    [current-shape (fill-cells state (cells-in-region (Posn 0 0) offset))])
                        (overlay/statemaps 
                            topology 
                            (Posn 0 0) 
                            current-shape
                            offset
                            (path-helper (rest segs))))]))
    (overlay/statemaps topology start (path-helper segs)))

(begin-for-syntax
  (define-syntax-class direction
    #:description "direction"
    (pattern (~or (~datum up) (~datum left) (~datum right) (~datum down)))))

;; Syntaxof -> SyntaxOf Listof (List S PositiveInteger)
(define-syntax (parse-path-seg stx)
    (syntax-parse stx
    [(_ type (state:expr magnitude:expr dir:direction more ...))
    #'(cons 
        (list state magnitude (ann (quote dir) Direction)) 
        (parse-path-seg type (state more ...)))]
    [(_ type (state:expr)) #''()]))

(define-syntax (parse-paths stx)
    (syntax-parse stx
        [(_ type path-seg:expr  ...)
            #'(append (parse-path-seg type path-seg) ...)]))

#|
(path (1 0) ['state1 5 up 3 right] ['state2 4 down 1 left])
->
(path-internal (Posn 1 0) 
        (list 
        (list 'state1 5 'up) (list 'state1 3 'right)) 
            (list 'state2 4 'down) (list 'state2 1 left) '())
|#
;; Draws a path as a statemap
(define-syntax (path stx)
    (syntax-parse stx
        [(_ (~datum :) type-name:id (x:expr y:expr) paths ...+)
            #'(ann (path-internal cartesian-topology 
                    (Posn x y) 
                    (parse-paths type-name paths ...)) (StateMap Posn type-name))]))

(module+ test
    (check-equal? 
        (path : AliveOrDead (1 -1) ('alive 2 right)) 
        (statemap-construct (list (cons (Posn 1 -1) 'alive) (cons (Posn 2 -1) 'alive))))
    (check-equal? 
        (path : AliveOrDead (-2 3) ('dead 3 up 5 right 3 down 1 left))
            (fill-cells 'dead (set (Posn -2 3) (Posn -2 2) (Posn -2 1) 
                (Posn -1 1) (Posn 0 1) (Posn 1 1) (Posn 2 1)
                (Posn 2 2) (Posn 2 3)))))

(module+ test
    (define-states states : FooStates (a b c))
    (check-equal? a 'a)
    (check-equal? b 'b)
    (check-equal? c 'c)
    (check-equal? states (list 'a 'b 'c)))

(provide define-states rect-custom rect-from overlay/statemaps rect-solid biased-random-select path)


