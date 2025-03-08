#lang typed/racket

(module+ test
  (require typed/rackunit)
  (require typed/2htdp/image)
  (require (submod "types.rkt" examples)))

(require typed/2htdp/image)
(require "types.rkt" )
(require "utils.rkt")

;; https://beautifulracket.com/explainer/modules.html

(define MAIN-DISPLAY-WIDTH 500)
(define MAIN-DISPLAY-HEIGHT 500)
(define BORDER-COLOR WHITE)

(define dummy-state-map (hash (Posn 0 0) 0 (Posn 1 0) 1))
(define dummy-world (World dummy-state-map cartesian-topology (lambda (c) #t)))


;; A simple 2d renderer which can translate a world where cells are represented by posn2ds and states are valid colors in 2htdp
(: 2d-renderer :  (All (S O) (->* ((World Posn O S) (ColorMap S)) (#:cell-width-px Positive-Integer #:origin Posn #:width-px Positive-Integer #:height-px Positive-Integer) Image)))
(define (2d-renderer world color-map #:cell-width-px [cell-width-px 30] #:origin [origin (Posn 0 0)] #:width-px [disp-width MAIN-DISPLAY-WIDTH] #:height-px [disp-height MAIN-DISPLAY-HEIGHT])
  (: draw-cell : (Posn -> Image))
  (define (draw-cell posn)
    (overlay
     (square cell-width-px "outline" BORDER-COLOR)
     (if (hash-has-key? (world-state-map world) posn)
         (square cell-width-px "solid" (color-map (hash-ref (world-state-map world) posn)))
         (text "-" (cast (floor (/ cell-width-px 2)) Positive-Byte) BORDER-COLOR))))
  (: draw-row : Integer -> Image)
  (define (draw-row cell-y)
    (foldr beside (draw-cell (Posn 100 cell-y)) (map draw-cell (map (lambda ([cell-x : Integer]) (Posn cell-x cell-y)) (range 0 99)))))
  (foldr above (draw-row 100) (map draw-row (range 0 99))))

(2d-renderer dummy-world (make-grayscale-color-map 0 2))

; (println example:STATEMAP-EMPTY)

;; https://docs.racket-lang.org/ts-reference/type-ref.html#%28form._%28%28lib._typed-racket%2Fbase-env%2Fbase-types-extra..rkt%29._-~3e%2A%29%29
