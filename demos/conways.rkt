#lang typed/racket
(define world : LifelikeWorld (random-world 50 50 ALIVE-OR-DEAD-STATES))
(define renderer : LifelikeRenderer (make-2d-renderer colormap-alive-or-dead))
(run world conways renderer)