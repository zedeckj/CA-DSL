#lang typed/racket
(require "../code/types.rkt" 
         "../code/run.rkt" 
         "../code/rule.rkt" 
         "../code/renderer.rkt" 
         "../code/library/statemaps.rkt" 
        "../code/library/colormaps.rkt" 
        "../code/library/topologies.rkt"
        "../code/library/neighborhoods.rkt")

(define conways : LifelikeRule
    (lifelike 
        [born 3]
        [survive 2 3]))

#;(define conways : LifelikeRule
    (moore-rule 
        #:state-type AliveOrDead
        [(dead -> alive) 3 in alive]
        [(alive -> alive) (2 3) in alive]
        [(_ -> dead)]))


#;(define conways : LifelikeRule
    (rule 
        #:cell-type Posn
        #:offset-type Posn
        #:state-type AliveOrDead
        #:neighborhood (moore-neighborhood)
        [(dead -> alive) 3 in alive]
        [(alive -> alive) (2 3) in alive]
        [(_ -> dead)]))


(define-2d-world world : AliveOrDead 
    #:state-map (rect-from 50 50
                (biased-random-select 
                    (list (cons alive 3) (cons dead 4)))))

(define renderer : LifelikeRenderer (make-2d-renderer colormap-alive-or-dead))
(run world conways renderer)