#lang typed/racket

;;; Example of Conway's Game of Life


(require 
    "code/main.rkt"
    "code/types.rkt"
    "code/rules.rkt"
    "code/renderer.rkt"
    "code/library/colormaps.rkt"
    "code/library/topologies.rkt"
    "code/library/neighborhoods.rkt"
    "code/library/statemaps.rkt"
    (for-syntax syntax/parse syntax/macro-testing))

#|
`lifelike` macro

<lifelike> ::= (lifelike <life-clause> ...)

<life-clause> ::= [<life-trans> <life-cond>]

<life-trans> ::= (<alive-or-dead> -> <alive-or-dead>)

<life-cond> ::= <number> in <alive-or-dead>
              | (<number> <number> ...) in <alive-or-dead>

<alive-or-dead> ::= 'alive
                  | 'dead
|#

#|
`rule` macro grammar
<rule> ::= 
    (rule 
        #:state-type <type>
        #:cell-type <type>
        #:offset-type <type>
        #:neighborhood <expr>
        <clauses>
    )

<clauses> ::= <clause> ... <default>

<default> ::= [default <expr>]

<clause> ::= [<transition> <condition>]

<transition> ::= (<expr> -> <expr>)

<condition> ::= <number> in <expr>
             | (<number> <number> ...) in <expr>

|#

#;(rule 
    #:state-type AliveOrDead
    #:cell-type Posn
    #:offset-type Posn
    #:neighborhood (moore-neighborhood)
    [('dead -> 'alive) 3 in 'alive]
    [('alive -> 'alive) (2 3) in 'alive]
    [default 'dead])
; ->  
#;(lambda  ([state-map : (StateMap Posn AliveOrDead)]
            [topology : (Topology AliveOrDead Posn)])
        (hash-map/copy state-map 
        (lambda ([cell : Posn]
                [in-state : AliveOrDead])
            (values cell
                (let ([neighbors : (Listof AliveOrDead) (get-neighbors cell state-map topology neighborhood)])
                    (let ([state-name : AliveOrDead 'alive]) ; for type checking, even if shortcircuiting
                            (if 
                                (and ((inst has-neighbors-in-state? AliveOrDead) state-name neighbors (list count ...)) (equal? state-name in-state))
                                state-name 
                                (parse-clauses neighbors state-type state clauses ...)))
                    (let ([state-name :  AliveOrDead 'dead]) state-name))))))

(define world : (World Posn Posn AliveOrDead) (random-world 50 50 ALIVE-OR-DEAD-STATES))
(define renderer : (Renderer Posn Posn AliveOrDead) (make-2d-renderer colormap-alive-or-dead))
(define conways : (Rule Posn Posn AliveOrDead)
    (rule 
        #:state-type AliveOrDead
        #:cell-type Posn
        #:offset-type Posn
        #:neighborhood (moore-neighborhood)
        [('dead -> 'alive) 3 in 'alive]
        [('alive -> 'alive) (2 3) in 'alive]
        [default 'dead])
  #;(lifelike 
    [('dead -> 'alive) 3 in 'alive]
    [('alive -> 'alive) (2 3) in 'alive]))

(run world conways renderer)


