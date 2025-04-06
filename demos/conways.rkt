#lang typed/racket
(require "../code/types.rkt" "../code/run.rkt" "../code/rule.rkt" "../code/renderer.rkt" "../code/library/statemaps.rkt" 
    "../code/library/colormaps.rkt" "../code/library/topologies.rkt")

(define conways : LifelikeRule
    (lifelike 
        [born 3]
        [survive 2 3]))

(define world : LifelikeWorld 
    (simple-world #:statemap 
        (ann (rect-random 50 50
                (biased-random-select 
                (ann (list (cons 'alive 1) (cons 'dead 3))
                    (Listof (Pairof AliveOrDead Nonnegative-Integer)))))
         (StateMap Posn AliveOrDead))))
(define renderer : LifelikeRenderer (make-2d-renderer colormap-alive-or-dead))
(run world conways renderer)