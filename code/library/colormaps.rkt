#lang typed/racket

(require typed/2htdp/image)
(module+ test (require typed/rackunit typed/2htdp/image))
(require "../types.rkt" "../utils.rkt")

(define BLACK (make-color 0 0 0))
(define WHITE (make-color 255 255 255))
(define RED (make-color 255 0 0))
(define GREEN (make-color 0 255 0))
(define BLUE (make-color 0 0 255))
(define YELLOW (make-color 255 255 0))
(define PURPLE (make-color 255 0 255))
(define PINK (make-color 255 200 200))
(define ORANGE (make-color 255 135 0))
(define GRAY (make-color 100 100 100))
(define TRANSPARENT (make-color 0 0 0 0))
(define COLOR_LIST (list WHITE BLACK RED GREEN BLUE YELLOW PURPLE PINK ORANGE))

(: colormap-alive-or-dead : (ColorMap AliveOrDead))
(define (colormap-alive-or-dead state)
  (match state
    ['alive BLACK]
    ['dead WHITE]))

(: make-default-colormap : (All (S) S * -> (ColorMap S)))
(define (make-default-colormap . states)
  (lambda ([state : Any])
    (: helper : (All (S) (S (Listof S) (Listof Color) -> Color)))
    (define (helper s states colors)
      (cond
        [(empty? states) (error "No color specified for state ~a" s)]
        [(equal? (first states) s) (first colors)]
        [else (helper s (rest states) (rest colors))]))
    (if (<= (length states) (length COLOR_LIST)) ;; TODO convert to contract
        (helper state states COLOR_LIST)
        (error "number of states exceeds number of predefined default colors"))))

;; TODO: Instead of taking a minimum and a maximum Integer values, 
;; make-grayscale-colormap can take a list of states of any type and map them to greyscale
(: make-grayscale-colormap : (Integer Integer -> (ColorMap Integer)))
(define (make-grayscale-colormap minimum maximum)
  (lambda ([state : Integer])
    (let* (
           [coeff : Exact-Rational (/ 255 (- maximum minimum))]
           [clamped : Integer (clamp state minimum maximum)]
           [scaled : Exact-Rational (floor (* coeff (- clamped minimum)))]
           [intensity : Byte (cast scaled Byte)])
      (make-color intensity intensity intensity))))
(module+ test
  (define g (make-grayscale-colormap 0 3))
  (check-equal? (g 0) (make-color 0 0 0))
  (check-equal? (g 1) (make-color 85 85 85))
  (check-equal? (g 2) (make-color 170 170 170))
  (check-equal? (g 3) (make-color 255 255 255))
  (check-equal? (g 4) (make-color 255 255 255)))


(provide (all-defined-out))