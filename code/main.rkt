#lang typed/racket
(require (rename-in typed/2htdp/universe [big-bang broken-big-bang])); See redefinition of big-bang
(require typed/2htdp/image)
(require (for-syntax syntax/parse) (for-syntax racket/syntax))
(require "types.rkt")
(require "utils.rkt")
(require "renderer.rkt")


(: id-rule : (All (C O S) (Rule C O S)))
;; An identity rule, which produces the same StateMap it recieves
(define (id-rule state-map topology) 
    state-map)

(: enforce-active-filter : (All (C S) (StateMap C S) (StateMap C S) (ActiveFilter C) -> (StateMap C S)))
;; Applies the given ActiveFilter to the provided StateMap, by setting cells that are inactive
;; to their previous state
(define (enforce-active-filter old-state-map new-state-map active-filter)
    (hash-map/copy new-state-map 
            (lambda ([cell : C] [state : S]) 
                (values cell 
                        (if (active-filter cell) 
                            state 
                            (hash-ref old-state-map cell))))))

(: tick-rule : (All (C O S) (World C O S) (Rule C O S) -> (World C O S)))
;; Applies the given rule to the provided world
(define (tick-rule world rule)
    (let ([active-filter (world-active-filter world)]
          [state-map (world-state-map world)]
          [topology (world-topology world)])
    (World (enforce-active-filter state-map (rule state-map topology) active-filter) 
            (world-topology world) 
            active-filter)))


;; The typed/2htdp/universe package incompletely replicates big-bang syntax by excluding the optional width and height arguments in the `to-draw` clause. This macro imperfectly approximates having those optional arguments by drawing a transparent rectangle of the appropriate size behind whatever the provided to-draw function draws. The size of the window is determined by the size of the image provided at tick 0. 
(define-syntax (big-bang stx)
    (syntax-parse stx
        [(_ initial-state (~datum :) type-state x ... [(~datum to-draw) body width height] y ...)
            (define/syntax-parse canvas #'(rectangle width height "solid" (make-color 0 0 0 0)))
            (define/syntax-parse new-to-draw #'(lambda ([ws : type-state]) 
                (overlay/xy (body ws) 0 0 canvas)))
            #'(broken-big-bang initial-state : type-state x ... [to-draw new-to-draw] y ...)]
        [(_ x ...) #'(broken-big-bang x ...)] ; Fall through to original syntax error of not having a to-draw clause
    ))

(: run : 
    (All (C O S) 
        (World C O S)
        (Rule C O S) 
        (Renderer C O S)
        -> 
        (World C O S)))
(define (run world rule renderer)
    (big-bang world :(World C O S)
        (name "CA Sim")
        [to-draw renderer 1000 1000]
        [on-tick (lambda ([ws : (World C O S)])  (tick-rule ws rule))]))


(run dummy-world id-rule 2d-renderer)
    ; #:to-draw (lambda (_) (square 2 "solid" "red"))
    ; [#:on-tick (lambda ([ws : Nonnegative-Integer]) (add1 ws))])


;; https://github.com/lexi-lambda/racket-2htdp-typed/blob/master/typed/2htdp/universe.rkt