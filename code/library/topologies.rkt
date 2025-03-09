#lang typed/racket
(require "../types.rkt" "../utils.rkt")
(module+ test (require typed/rackunit))

(: moore-neighborhood-outline : (Positive-Integer -> (Neighborhood Posn)))
(define (moore-neighborhood-outline distance)
    (: helper : Nonnegative-Integer -> (Setof Posn))
    (define (helper i)
        (set (Posn distance i) (Posn (* -1 distance) i) 
            (Posn i distance) (Posn (* -1 distance) i)))
    (define l : (Listof Nonnegative-Integer) (range 1 (add1 distance)))
    (define nested : (Listof (Setof Posn)) (map helper l))
    (apply set-union (first nested) (rest nested)))

(: moore-neighborhood (->* () (Positive-Integer) (Neighborhood Posn)))
(define (moore-neighborhood [distance 1])
    (define layer-indexes : (Listof Nonnegative-Integer) (range 1 (add1 distance)))
    (define nested : (Listof (Setof Posn)) 
        (map moore-neighborhood-outline (cast layer-indexes (Listof Positive-Integer))))
    (apply set-union (first nested) (rest nested)))

(module+ test 
    (check-equal? (moore-neighborhood) (list (Posn -1 -1) (Posn -1 0) (Posn -1 1)
        (Posn 0 -1) (Posn 0 1)
        (Posn 1 -1) (Posn 1 0) (Posn 1 1)))
    #;(check-equal? (moore-neighborhood 2) 
        (set-union (moore-neighborhood) 
            (list (Posn -2 -2) (Posn -2 -1) (Posn (-2)))
        )))

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
(module+ test 
    (define w (make-wrapping-cartesian-topology 0 10 0 10))
    (check-equal? (w (Posn 5 5) (Posn 1 1)) (Posn 6 6))
    (check-equal? (w (Posn 0 10) (Posn 1 1)) (Posn 1 1))
)

( : init-2d-world : (All (S) Positive-Integer Positive-Integer (Posn -> S) -> (World Posn Posn S)))
(define (init-2d-world max-x max-y state-initializer)
    (make-world 
        (for*/hash : (StateMap Posn S) 
            ([x (range max-x)] 
             [y (range max-y)])
            (values (Posn x y) (state-initializer (Posn x y))))
        (make-finite-cartesian-topology max-x max-y)
        (lambda ([_ : Posn]) #t)))


( : random-world : (All (S) (->* (Positive-Integer Positive-Integer (Listof S)) (#:seed (Union False Positive-Integer)) (World Posn Posn S))))
(define (random-world max-x max-y states #:seed (seed #f))
    (define start-rand-x 10)
    (define start-rand-y 10)
    (define end-rand-x (/ (* 3 max-x) 4))
    (define end-rand-y (/ (* 3 max-y) 4))
    (when seed
        (random-seed seed))
    (: random-state (Posn -> S))
    (define (random-state pos)
        (let ([x (posn-x pos)]
              [y (posn-y pos)])
            (if (and (>= x start-rand-x) (<= x end-rand-x) (>= y start-rand-y) (<= y end-rand-y))
                (list-ref states (random 0 (length states)))
                (first states))))
    (init-2d-world max-x max-y random-state))

(provide (all-defined-out))
