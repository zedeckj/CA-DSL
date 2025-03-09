#lang typed/racket

(require "types.rkt")
(module+ test (require typed/rackunit))

(require typed/2htdp/image)

;; List abstraction which augments a list with indexes, similar to `enumerate` in Python.  
(: enumerate : (All (T) ((Listof T) -> (Listof (Pairof Nonnegative-Integer T)))))
(define (enumerate lst)
    (define f : (Nonnegative-Integer T -> (Pairof Nonnegative-Integer T)) (lambda (i v) (cons i v)))
    (map f (range (length lst)) lst))
(module+ test
    (check-equal? (enumerate '()) '())
    (check-equal? (enumerate (list 5)) (list (cons 0 5)))
    #;(check-equal? (enumerate (list 'one 'two 'three) #:starting-index 1) 
        (list (cons 1 'one) (cons 2 'two) (cons 3 'three)))
)

; (define-type (All (T) (kleene) (Or ))) https://docs.racket-lang.org/ts-guide/types.html#(part._.Uniform_.Variable-.Arity_.Functions)

;; Utility constructor for a simple rectangular grids. Accepts a list of rows going from top to bottom, where each row is a list of states going from left to right. Ragged grids are ok. 
(: make-statemap-2d : (All (S) (-> (Listof S) * (StateMap Posn S))))
(define (make-statemap-2d . rows)
    (define ret : (StateMap Posn S) (make-hash))
    (: make-row-of-states : (Pairof Nonnegative-Integer (Listof S)) -> Void)
    (define (make-row-of-states row-with-index)
        (match-define (cons row-index row) row-with-index)
        (: handle-cell : (Pairof Nonnegative-Integer S) -> (Pairof Posn S))
        (define (handle-cell cell-with-index)
            (match-define (cons col-index state) cell-with-index)
            (cons (Posn col-index row-index) state))
        (for ([v (map handle-cell (enumerate row))]) 
            (hash-set! ret (car v) (cdr v))))
    (for ([v (enumerate rows)])
        (make-row-of-states v))
    (hash-map/copy ret (lambda ([k : Posn] [v : S]) (values k v)) #:kind 'immutable))
(module+ test
    (check-equal? (make-statemap-2d '(0-0 1-0) '(0-1) '(0-2 1-2 2-2))
        (hash 
            (Posn 0 0) '0-0 (Posn 1 0) '1-0
            (Posn 0 1) '0-1
            (Posn 0 2) '0-2 (Posn 1 2) '1-2 (Posn 2 2) '2-2))
) 
;; (equal? (hash 0 1 1 2) (hash 1 2 0 1)) -> #t
;; (equal? (hash 0 (Posn 0 0) 1 (Posn 0 1)) (hash  1 (Posn 0 1) 0 (Posn 0 0))) -> #t


(define BLACK (make-color 0 0 0))
(define WHITE (make-color 255 255 255))
(define RED (make-color 255 0 0))
(define GREEN (make-color 0 255 0))
(define BLUE (make-color 255 0 255))
(define YELLOW (make-color 255 255 0))
(define PURPLE (make-color 255 0 255))
(define PINK (make-color 255 200 200))
(define ORANGE (make-color 255 135 0))
(define COLOR_LIST (list WHITE BLACK RED GREEN BLUE YELLOW PURPLE PINK ORANGE))

(: make-default-color-map : (All (S) (Listof S) -> (ColorMap S)))
(define (make-default-color-map . states)
    (lambda ([state : Any])
        (: helper : (All (S) (S (Listof S) (Listof Color) -> Color)))
        (define (helper s states colors)
            (cond 
            [(empty? states) (error "invalid state")]
            [(equal? (first states) s) (first colors)]
            [else (helper s (rest states) (rest colors))]))
        (if (<= (length states) (length COLOR_LIST)) ;; TODO convert to contract
            (helper state states COLOR_LIST)
            (error "too many states"))))




(: clamp : Integer Integer Integer -> Integer)
(define (clamp val min-val max-val)
  (cond
    [(> val max-val) max-val]
    [(< val min-val) min-val]
    [else val]))

(: wrap : Integer Integer Integer -> Integer)
(define (wrap val min-val max-val)
  (cond
    [(> val max-val) (wrap (+ min-val (- val max-val)) min-val max-val)]
    [(< val min-val) (wrap (- max-val (- min-val val)) min-val max-val)]
    [else val]))

(: make-grayscale-color-map : Integer Integer -> (ColorMap Integer))
(define (make-grayscale-color-map minimum maximum)
  (let ([coeff (/ 255 (- maximum minimum))])
    (lambda ([state : Integer])
        (let ([intensity (cast (floor (* coeff (- (clamp state minimum maximum) minimum))) Byte)])
          (make-color intensity intensity intensity)))))


(: cartesian-topology : (Topology Posn Posn))
(define (cartesian-topology pos offset)
    (Posn (+ (posn-x offset) (posn-x pos))
          (+ (posn-y offset) (posn-y pos))))

(: truncate-topology : (All (C O) ((Topology C O ) (C O C -> Boolean ) -> (Topology C O))))
(define (truncate-topology topology predicate)
    (lambda ([cell : C ] [offset : O])
         (let ([out (topology cell offset)])
             (unless (void? out)
                (when (predicate cell offset out)
                    out)))))

; (define (wrap-x))
; (define (flip-wrap-x))
; (define (wrap-y))
; (define (flip-wrap-y))

(: modify-topology : (All (C O) ((Topology C O) (C -> (Union Void C)) *  -> (Topology C O))))
(define (modify-topology topology . modifiers)
  (lambda (cell offset)
    (: nest : (-> (Union C Void) (Listof (C -> (Union Void C))) (Union C Void)))
    (define (nest cell modifiers)
        (cond 
            [(or (empty? modifiers) (void? cell)) cell]
            [else (nest ((first modifiers) cell) (rest modifiers))]))
    (nest (topology cell offset) modifiers)))
            
(: make-finite-cartesian-topology : (Positive-Integer Positive-Integer -> (Topology Posn Posn)))
#|
out = topology(cell)
if something(out):
    out
else:
    void
|#
(define (make-finite-cartesian-topology max-x max-y)
    (truncate-topology cartesian-topology 
        (lambda (_ __ [new-pos : Posn]) (in-cartesian-region new-pos (Posn max-x max-y)))))
        ;or (> (x new-pos) max-x) (> y max-y) (< y 0) (< x 0)))
    #;(lambda ([pos : Posn] [offset : Posn])
        (let* (
            [new-pos (cartesian-topology pos offset)]
            [x (posn-x new-pos)]
            [y (posn-y new-pos)])
        (unless (or (> (x new-pos) max-x) (> y max-y) (< y 0) (< x 0))
            new-pos)))

;; Returns if a point is in a region bounded by the origin and the provided point
(: in-cartesian-region : (->* (Posn Posn) [#:origin Posn] Boolean))
(define (in-cartesian-region point max-point #:origin [origin (Posn 0 0)])
    (let ([x (posn-x point)]
            [y (posn-y point)])
        (or (> x (posn-x max-point)) (> y (posn-y max-point)) (< y (posn-y origin)) (< x (posn-x origin)))))


(: make-wrapping-cartesian-topology : (Integer Integer Integer Integer -> (Topology Posn Posn)))
(define (make-wrapping-cartesian-topology x-min x-max y-min y-max)
  (modify-topology
   cartesian-topology
   (lambda ([pos : Posn])
        (Posn (wrap (posn-x pos) x-min x-max) (wrap (posn-y pos) y-min y-max)))))

( : init-2d-world : (All (S) Positive-Integer Positive-Integer (Posn -> S) -> (World Posn Posn S)))
(define (init-2d-world max-x max-y state-initializer)
    (make-world 
        (for/hash : (StateMap Posn S) 
            ([x (range max-x)] 
             [y (range max-y)])
            (values (Posn x y) (state-initializer (Posn x y))))
        (make-finite-cartesian-topology max-x max-y)
        (lambda ([_ : Posn]) #t)))

          
(module+ test
    (define g (make-grayscale-color-map 0 3))
    (check-equal? (g 0) (make-color 0 0 0)) 
    (check-equal? (g 1) (make-color 85 85 85))
    (check-equal? (g 2) (make-color 170 170 170))
    (check-equal? (g 3) (make-color 255 255 255))
    (check-equal? (g 4) (make-color 255 255 255)))
    
(module+ test 
    (define w (make-wrapping-cartesian-topology 0 10 0 10))
    (check-equal? (w (Posn 5 5) (Posn 1 1)) (Posn 6 6))
    (check-equal? (w (Posn 0 10) (Posn 1 1)) (Posn 1 1))
)
    


(provide (all-defined-out))