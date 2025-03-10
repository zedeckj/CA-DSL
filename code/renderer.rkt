#lang typed/racket

(module+ test
  (require typed/rackunit typed/2htdp/image)
  (require "expected-images.rkt" "examples.rkt"))

(require typed/2htdp/image)
(require "types.rkt" "library/colormaps.rkt")

(define MAIN-DISPLAY-WIDTH 2000)
(define MAIN-DISPLAY-HEIGHT 2000)
(define BORDER-COLOR BLACK)

;; Creates a simple 2d renderer which can translate a world where cells are represented by Posns to a graphical representation.
;; TODO Future improvements: Show a visual indication for when cells have non-standard neighbors and when cells are frozen.
(: make-2d-renderer : 
  (All (O S) 
    (->* ((ColorMap S)) 
      (#:cell-width-px Positive-Integer 
        #:origin-px Posn 
        #:width-px Positive-Integer 
        #:height-px Positive-Integer)  
      (Renderer Posn O S))))
(define (make-2d-renderer color-map 
  #:cell-width-px [cell-width-px 30] 
  #:origin-px [origin-px (Posn 0 0)] 
  #:width-px [disp-width MAIN-DISPLAY-WIDTH] 
  #:height-px [disp-height MAIN-DISPLAY-HEIGHT])

  ;; Computing the cropping in terms of pixels and cells
  (: first-idx : Real -> Integer)
  (define (first-idx origin-px-component) 
    (cast (floor (/ origin-px-component cell-width-px)) Integer))
  (: last-idx : Real Real -> Integer)
  (define (last-idx origin-px-component disp-len) 
    (cast (ceiling (/ (+ origin-px-component disp-len) cell-width-px)) Integer))
  (define first-row-idx (first-idx (posn-y origin-px)))
  (define first-col-idx (first-idx (posn-x origin-px)))
  (define last-col-idx (last-idx (posn-x origin-px) disp-width))
  (define last-row-idx (last-idx (posn-y origin-px) disp-height))
  (define crop-left (- (posn-x origin-px) (* cell-width-px first-col-idx)))
  (define crop-top (- (posn-y origin-px) (* cell-width-px first-col-idx)))


  (: renderer : (All (O) (Renderer Posn O S)))
  (define (renderer world)
    (: draw-cell : (Posn -> Image))
    (define (draw-cell posn)
      (overlay
       (square cell-width-px "outline" BORDER-COLOR)
       (if (hash-has-key? (world-state-map world) posn)
           (square cell-width-px "solid" (color-map (hash-ref (world-state-map world) posn)))
           (text "-" (cast (floor (/ cell-width-px 2)) Positive-Byte) BORDER-COLOR))))

    (: draw-row : Integer -> Image)
    (define (draw-row cell-y)
      (foldr beside (draw-cell (Posn last-col-idx cell-y)) (map draw-cell (map (lambda ([cell-x : Integer]) (Posn cell-x cell-y)) (range first-col-idx (sub1 last-col-idx))))))

    (crop crop-left crop-top disp-width disp-height
          (foldr above (draw-row last-row-idx)
                 (map draw-row (range first-row-idx (sub1 last-row-idx))))))
  renderer)


(module+ test
  (: render-dummy-with-offset : (Posn -> Image))
  (define (render-dummy-with-offset offset)
    (define cm : (ColorMap Integer) (make-grayscale-color-map 0 2))
    (define renderer : (Renderer Posn Posn Integer) (make-2d-renderer cm #:origin-px offset))
    ;(define world : (World Posn Any Any) dummy-world)
    (renderer WORLD-CART-2CELL))
  (check-equal? (render-dummy-with-offset (Posn 1 1)) dummy-world-no-top-or-left-border)
  (check-equal? (render-dummy-with-offset (Posn 28 29)) dummy-world-28-29)
  (check-equal? (render-dummy-with-offset (Posn 0 0)) dummy-world-at-origin)
  (check-equal? (render-dummy-with-offset (Posn 0 1)) dummy-world-no-top-border)
  (check-equal? (render-dummy-with-offset (Posn 1 0)) dummy-world-no-left-border)
  )

(provide make-2d-renderer)









; ; (println example:STATEMAP-EMPTY)
; ;; https://docs.racket-lang.org/rackcheck/index.html#%28def._%28%28lib._rackcheck%2Fmain..rkt%29._property~3f%29%29
; #;(check-property
;    (property ([world (gen:world)])
;              (check-equal? (2d-renderer world (make-grayscale-color-map 0 2) #:origin (Posn 1 1) )))
;    )


; ;TODO reorg

; ;; https://docs.racket-lang.org/ts-reference/type-ref.html#%28form._%28%28lib._typed-racket%2Fbase-env%2Fbase-types-extra..rkt%29._-~3e%2A%29%29
