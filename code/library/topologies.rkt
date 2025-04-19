#lang typed/racket
(require "../types.rkt" 
        "../utils.rkt"
        (for-syntax syntax/parse))
(module+ test (require typed/rackunit))


;; A standard topology which add Posns linearly 
(: cartesian-topology : (Topology Posn Posn))
(define (cartesian-topology pos offset)
    (Posn (+ (posn-x offset) (posn-x pos))
          (+ (posn-y offset) (posn-y pos))))

;; Restricts a Topology to produce additional Void returns an input cell and offset, and the output of 
;; the original topology to not satisfy the provided predicate
(: truncate-topology : (All (C O) ((Topology C O ) (C O C -> Boolean ) -> (Topology C O))))
(define (truncate-topology topology predicate)
    (lambda ([cell : C ] [offset : O])
         (let ([out (topology cell offset)])
             (unless (void? out)
                (when (predicate cell offset out)
                    out)))))



;; Modifies a Topology with a series of "modifiers", which each take in a Cell and produce either
;; a Void or a new Cell value, which are applied in sequence to outputs of a topology 
(: modify-topology : (All (C O) ((Topology C O) (C -> (Union Void C)) *  -> (Topology C O))))
(define (modify-topology topology . modifiers)
  (lambda (cell offset)
    (: nest : (-> (Union C Void) (Listof (C -> (Union Void C))) (Union C Void)))
    (define (nest cell modifiers)
        (cond 
            [(or (empty? modifiers) (void? cell)) cell]
            [else (nest ((first modifiers) cell) (rest modifiers))]))
    (nest (topology cell offset) modifiers)))
            

;; Creates a modified cartesian topology which is restricted by the given max-x and max-y values. Outputs cells 
;; from the cartesian topology which have x values or y values with absolute values greater than the given
;; max-x or max-y are turned to Void.
(: make-finite-cartesian-topology : (Positive-Integer Positive-Integer -> (Topology Posn Posn)))
(define (make-finite-cartesian-topology max-x max-y)
    (truncate-topology cartesian-topology 
        (lambda (_ __ [new-pos : Posn]) (in-cartesian-region new-pos (Posn max-x max-y)))))

;; Returns if a point is in a region bounded by the origin and the provided point
(: in-cartesian-region : (->* (Posn Posn) [#:origin Posn] Boolean))
(define (in-cartesian-region point max-point #:origin [origin (Posn 0 0)])
    (let ([x (posn-x point)]
            [y (posn-y point)])
        (not (or (> x (posn-x max-point)) (> y (posn-y max-point)) (< y (posn-y origin)) (< x (posn-x origin))))))

;; Creates a modified cartesian topology in which Posns outputs are "wrapped" around at the given
;; values
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

;; Convienence functions for creating 2DWorlds with bounded cartesian topolgies. 
;; The state-initializer function is used to set the starting State of each Cell in the StateMap of
;; the world 
( : init-2d-world : (All (S) Positive-Integer Positive-Integer (Posn -> S) -> (2DWorld S)))
(define (init-2d-world max-x max-y state-initializer)
    (define ht : (StateMap Posn S) (make-hash))
    (for* 
        ([x (range max-x)] 
            [y (range max-y)])
        (hash-set! ht (Posn x y) (state-initializer (Posn x y))))
    (make-world 
        ht
        (make-finite-cartesian-topology max-x max-y)
        (lambda ([_ : Posn]) #t)))


;; Constructs a 2DWorld with default arguments of a standard Cartesian Topology and an ActiveFilter
;; which returns true for all Cells
(: simple-2d-world : (All (S) (->* (#:state-map [StateMap Posn S]) 
                                   (#:active-filter [ActiveFilter Posn] 
                                    #:topology [Topology Posn Posn]) 
                                    [World Posn Posn S])))
(define (simple-2d-world #:state-map statemap 
                         #:topology [topology cartesian-topology] 
                         #:active-filter [active-filter (lambda (_) #t)]) 
    (World statemap topology active-filter) )


(define-syntax (define-2d-world stx)
    (syntax-parse stx
    [(_ world:id (~datum :) state-type:id 
        #:state-map statemap:expr 
        (~optional (~seq #:active-filter active-filter:expr))
        (~optional (~seq #:topology topology:expr)))
    #'(define world : (2DWorld state-type)
        (simple-2d-world 
            #:state-map (ann statemap (StateMap Posn state-type))
            (~? (~@ #:active-filter active-filter))
            (~? (~@ #:topology topology))))]))

(provide (all-defined-out))
