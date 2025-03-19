#lang typed/racket
(require "../types.rkt" "../utils.rkt")
(module+ test (require typed/rackunit))


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
        (not (or (> x (posn-x max-point)) (> y (posn-y max-point)) (< y (posn-y origin)) (< x (posn-x origin))))))

(: make-wrapping-cartesian-topology : (Integer Integer Integer Integer -> (Topology Posn Posn)))
(define (make-wrapping-cartesian-topology x-min x-max y-min y-max)
  (modify-topology
   cartesian-topology
   (lambda ([pos : Posn])
        (Posn (wrap (posn-x pos) x-min x-max) (wrap (posn-y pos) y-min y-max)))))
(module+ test 
    (define w (make-wrapping-cartesian-topology 0 10 0 10))
    (check-equal? (w (Posn 5 5) (Posn 1 1)) (Posn 6 6))
    (check-equal? (w (Posn 0 10) (Posn 1 1)) (Posn 1 0))
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
    (when seed
        (random-seed seed))
    (init-2d-world max-x max-y 
                    (lambda (_) 
                    (list-ref states (random 0 (length states))))))

;(: generate-random : )


(provide (all-defined-out))
