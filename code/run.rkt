#lang typed/racket

;;; Functionality to display a window 

(require (for-syntax syntax/parse) (for-syntax racket/syntax))
(require typed/2htdp/image)
(require "types.rkt" 
        "renderer.rkt" 
        "library/colormaps.rkt" 
        "library/topologies.rkt" 
        "rule.rkt")
(module+ test (require rackunit))
(require (rename-in typed/2htdp/universe [big-bang broken-big-bang]))
;; The typed/2htdp/universe package incompletely replicates big-bang syntax by excluding the optional width and height arguments in the `to-draw` clause. This macro imperfectly approximates having those optional arguments by drawing a transparent rectangle of the appropriate size behind whatever the provided to-draw function draws. In big-bang, the size of the window is determined by the size of the image provided at tick 0. 
(define-syntax (big-bang stx)
    (syntax-parse stx
        [(_ initial-state (~datum :) type-state x ... [(~datum to-draw) body width height] y ...)
            (define/syntax-parse canvas #'(rectangle width height "solid" (make-color 0 0 0 0)))
            (define/syntax-parse new-to-draw 
                #'(lambda ([ws : type-state]) 
                    (let* (
                        [renderer-image (body ws)]
                        [scale-factor-x (/ (image-width canvas) (image-width renderer-image))]
                        [scale-factor-y (/ (image-height canvas) (image-height renderer-image))])
                    (when (or (zero? scale-factor-x) (zero? scale-factor-y)) (error "Cannot pass in 0 width or height"))
                    (overlay/xy (scale/xy scale-factor-x scale-factor-y renderer-image ) 0 0 canvas))))
            #'(broken-big-bang initial-state : type-state x ... [to-draw new-to-draw] y ...)]
        [(_ x ...) #'(broken-big-bang x ...)] ; Fall through to original syntax error of not having a to-draw clause
    ))


;; Applies the given ActiveFilter to the provided StateMap, by setting cells that are inactive
;; to their previous state
(: enforce-active-filter : (All (C S) (StateMap C S) (StateMap C S) (ActiveFilter C) -> (StateMap C S)))
(define (enforce-active-filter old-state-map new-state-map active-filter)
    (hash-map/copy new-state-map 
            (lambda ([cell : C] [state : S]) 
                (values cell 
                        (if (active-filter cell) 
                            state 
                            (hash-ref old-state-map cell))))))

;; Applies the given rule to the provided world
(: tick-rule : (All (C O S) (World C O S) (Rule C O S) -> (World C O S)))
(define (tick-rule world rule)
    (let ([active-filter (world-active-filter world)]
          [state-map (world-state-map world)]
          [topology (world-topology world)])
    (World (enforce-active-filter state-map (rule state-map topology) active-filter) 
            (world-topology world) 
            active-filter)))

;; Opens a native window to visualize the cellular automata
(: run : 
    (All (C O S) 
        (World C O S)
        (Rule C O S) 
        (Renderer C O S)
        -> 
        (World C O S)))
(define (run world rule renderer)
    (big-bang world : (World C O S)
        (name "CA Sim")
        [to-draw renderer WINDOW-WIDTH WINDOW-HEIGHT]
        [on-tick (lambda ([ws : (World C O S)]) (tick-rule ws rule)) 1]))

(provide run)



;;;;;;;;;; GRAVEYARD ;;;;;;;;
#;(define-syntax (run-macro stx)
    (syntax-parse stx 
        [(_ (cell-type:id offset-type:id state-type:id) 
            world:expr rule:expr renderer:expr)
        #'(run 
            (cast world (World cell-type offset-type state-type)) 
            (cast rule (Rule cell-type offset-type state-type))
            (cast renderer (Renderer cell-type offset-type state-type)))]))

         #;#'(begin 
                (define ann-world : (World cell-type offset-type state-type) world) 
                (define ann-rule : (Rule cell-type offset-type state-type) rule) 
                (define ann-renderer : (Renderer cell-type offset-type state-type) renderer)
            #;(run ann-world ann-rule ann-renderer))

#;(define renderer : (Renderer cell-type offset-type state-type) (make-2d-renderer colormap-alive-or-dead))
#;(define states : (Listof AliveOrDead) (list 'dead 'alive))
#;(define world : (World cell-type offset-type state-type) (random-world 50 50 states))
#;(run world conways renderer)
#;(define-rule some-rule : (Rule Posn Posn AliveOrDead)
  (rule 
    [('dead -> 'alive) 4 in 'alive]
    [('alive -> 'alive) (2 5) in 'alive]
    [default 'dead]))

#;(define renderer : (Renderer Posn Posn AliveOrDead) (make-2d-renderer colormap-alive-or-dead))
#;(define states : (Listof AliveOrDead) (list 'dead 'alive))
#;(define world : (World Posn Posn AliveOrDead) (random-world 50 50 states))
#;(run world conways renderer)

#;(define world ())