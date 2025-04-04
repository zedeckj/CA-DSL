#lang typed/racket
   ;;; Example of Conway's Game of Life
   
   (require 
       "../code/run.rkt"
       "../code/types.rkt"
       "../code/rule.rkt"
        "../code/renderer.rkt"
        "../code/library/colormaps.rkt"
        "../code/library/topologies.rkt"
        "../code/library/neighborhoods.rkt"
        "../code/library/statemaps.rkt"
       (for-syntax syntax/parse syntax/macro-testing))

   ;; In CA, this is often expressed as "B3/S23"
#;(define conways : LifelikeRule
   (rule
       #:cell-type Posn
       #:offset-type Posn
       #:state-type AliveOrDead
       #:neighborhood (moore-neighborhood)
       [('dead -> 'alive) 3 in 'alive]
       [('alive -> 'alive) (2 3) in 'alive]
       [(_ -> 'dead)]))

(define conways : LifelikeRule
    (lifelike 
        [born 3]
        [survive 2 3]))



(define world : LifelikeWorld (random-world 50 50 ALIVE-OR-DEAD-STATES))
(define renderer : LifelikeRenderer (make-2d-renderer colormap-alive-or-dead))
(run world conways renderer)



   #|
; expansion of lifelike:
#;(rule 
    #:cell-type Posn
    #:offset-type Posn
    #:state-type AliveOrDead
    #:neighborhood (moore-neighborhood)
    [('dead -> 'alive) 3 in 'alive]
    [('alive -> 'alive) (2 3) in 'alive]
    [default 'dead])


   ; rule roughly further expands to 
   #;(define (conway-rule state-map topology)
       (mapper state-map
           (lambda ([cell : Posn]
                   [in-state : AliveOrDead])
               (let ([neighbors (get-neighbors cell state-map topology (moore-neighborhood))])
                   (match in-state
                       ['alive (if (has-neighbors-in-state? 'alive neighbors '(2 3)) 'alive 'dead)]
                       ['dead (if (has-neighbors-in-state? 'alive neighbors '(3))'alive 'dead)])))))
   
   #|
   `lifelike` macro
   
   <lifelike> ::= (lifelike 
           [born <number> <number> ...]
           [survive <number> <number> ...])
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
|#
