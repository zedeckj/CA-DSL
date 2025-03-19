#lang typed/racket

;; Example of Conway's Game of Life


(require 
    "code/main.rkt"
    "code/types.rkt"
    "code/macros.rkt"
    "code/renderer.rkt"
    "code/library/colormaps.rkt"
    "code/library/topologies.rkt"
    "code/library/neighborhoods.rkt"
    (for-syntax syntax/parse))

(define-syntax (run-macro stx)
    (syntax-parse stx 
        [(_ type-world 
            world:expr rule:expr renderer:expr)
            (println "phase 1")
            (define foo #'(begin 
                (println "hello world")
                (define-values (cell-type offset-type state-type) (apply values type-world))
                (define ann-world : (World cell-type offset-type state-type) world) 
                (define ann-rule : (Rule cell-type offset-type state-type) rule) 
                (define ann-renderer : (Renderer cell-type offset-type state-type) renderer)
            (run ann-world ann-rule ann-renderer)))
            (println (syntax->datum foo))
            foo]))

(define renderer : (Renderer Posn Posn AliveOrDead) (make-2d-renderer colormap-alive-or-dead))
(define states : (Listof AliveOrDead) (list 'dead 'alive))
(define world : (World Posn Posn AliveOrDead) (random-world 50 50 states))

(define Lifelike '(Posn Posn AliveOrDead))


(define my-conways : (Rule Posn Posn AliveOrDead)
  (rule 
    #:state-type AliveOrDead
    #:cell-type Posn
    #:offset-type Posn
    #:neighborhood (moore-neighborhood)
    [('dead -> 'alive) 3 in 'alive]
    [('alive -> 'alive) (2 3) in 'alive]
    [default 'dead]))

(run-macro (Posn Posn AliveOrDead) world my-conways renderer)