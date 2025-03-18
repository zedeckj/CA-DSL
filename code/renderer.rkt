#lang typed/racket

(module+ test
  (require typed/rackunit typed/2htdp/image)
  (require "expected-images.rkt" "examples.rkt"))

(require typed/2htdp/image)
(require "types.rkt" "library/colormaps.rkt")

;; Defines the dimensions of the big bang window
(define DISPLAY-WIDTH 1000)
(define DISPLAY-HEIGHT 1000)

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
  #:origin-px [origin-px (Posn 0 0)] )

  (define BORDER-COLOR BLACK)
  (define UNDEFINED-COLOR GRAY) 

  ;; Computing the cropping in terms of pixels and cells
  (: first-idx : Real -> Integer)
  (define (first-idx origin-px-component) 
    (cast (floor (/ origin-px-component cell-width-px)) Integer))
  (: last-idx : Real Real -> Integer)
  (define (last-idx origin-px-component disp-len) 
    (cast (ceiling (/ (+ origin-px-component disp-len) cell-width-px)) Integer))
  (define first-row-idx (first-idx (posn-y origin-px)))
  (define first-col-idx (first-idx (posn-x origin-px)))
  (define last-col-idx (last-idx (posn-x origin-px) MAIN-DISPLAY-WIDTH))
  (define last-row-idx (last-idx (posn-y origin-px) MAIN-DISPLAY-HEIGHT))
  (define crop-left (- (posn-x origin-px) (* cell-width-px first-col-idx)))
  (define crop-top (- (posn-y origin-px) (* cell-width-px first-col-idx)))

  ;; helper functions for this renderer
  (define undefined-cell-img : Image 
    (overlay
      (square cell-width-px "outline" UNDEFINED-COLOR)
        (text "x" (cast (floor (/ cell-width-px 2)) Positive-Byte) UNDEFINED-COLOR)))

  (: cell-img : (Color -> Image))
  (define (cell-img color )  
    (overlay
      (square cell-width-px "outline" BORDER-COLOR)
        (square cell-width-px "solid" color)))

  ;; Function that gets returned
  ;(: renderer : (All (O) (Renderer Posn O S)))
  (lambda ([world : (World Posn O S)])
  
    (: draw-cell : (Posn -> Image))
    (define (draw-cell posn)
       (if (hash-has-key? (world-state-map world) posn) 
        (cell-img (color-map (hash-ref (world-state-map world) posn)) )
        undefined-cell-img))
  
    (: draw-row : Integer -> Image)
    (define (draw-row cell-y)
      (foldr beside (draw-cell (Posn last-col-idx cell-y)) 
        (map draw-cell 
          (map 
            (lambda ([cell-x : Integer]) (Posn cell-x cell-y)) 
            (range first-col-idx (sub1 last-col-idx))))))

    (crop crop-left crop-top DISPLAY-WIDTH DISPLAY-HEIGHT
          (foldr above (draw-row last-row-idx)
                 (map draw-row (range first-row-idx (sub1 last-row-idx)))))))


(module+ test
  (: render-dummy-with-offset : (Posn -> Image))
  (define (render-dummy-with-offset offset)
    (define cm : (ColorMap Integer) (make-grayscale-colormap 0 2))
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