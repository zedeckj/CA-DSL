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
    [(> val max-val) (+ (- val max-val) min-val)]
    [(< val min-val) (- max-val (- min-val val))]
    [else val]))

(: make-grayscale-color-map : Integer Integer -> (ColorMap Integer))
(define (make-grayscale-color-map minimum maximum)
  (let ([coeff (/ 255 (- maximum minimum))])
    (lambda ([state : Integer])
        (let ([intensity (cast (floor (* coeff (- (clamp state minimum maximum) minimum))) Byte)])
          (make-color intensity intensity intensity)))))


(: cartesian-topology : (Posn Posn -> Posn))
(define (cartesian-topology pos offset)
    (Posn (+ (posn-x pos) (posn-x offset)) (+ (posn-y pos) (posn-y offset))))

(: make-wrapping-cartesian : (Integer Integer Integer Integer -> (Posn Posn -> Posn)))
(define (make-wrapping-cartesian x-min x-max y-min y-max)
  (lambda ([pos : Posn] [offset : Posn])
    (let* ([out (cartesian-topology pos offset)]
           [x (posn-x out)]
           [y (posn-y out)])
      (Posn (wrap x x-min x-max) (wrap y y-min y-max)))))


          
(module+ test
    (define g (make-grayscale-color-map 0 3))
    (check-equal? (g 0) (make-color 0 0 0)) 
    (check-equal? (g 1) (make-color 85 85 85))
    (check-equal? (g 2) (make-color 170 170 170))
    (check-equal? (g 3) (make-color 255 255 255))
    (check-equal? (g 4) (make-color 255 255 255)))
    
(module+ test 
    (define w (make-wrapping-cartesian 0 10 0 10))
    (check-equal? (w (Posn 5 5) (Posn 1 1)) (Posn 6 6))
    (check-equal? (w (Posn 0 10) (Posn 1 1)) (Posn 1 1))
)
    


(provide (all-defined-out))