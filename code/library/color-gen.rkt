#lang typed/racket
(require typed/racket/stream typed/2htdp/image)
(module+ test (require typed/rackunit))

(define-type Point (List Real Real Real))

(: cube-faces (Listof Point))
(define cube-faces
  (list
   '(0 0.5 0.5) '(1 0.5 0.5)
   '(0.5 0 0.5) '(0.5 1 0.5)
   '(0.5 0.5 0) '(0.5 0.5 1)))

(: cube-vertices (Listof Point))
(define cube-vertices
  (list
   '(1 1 1) '(0 0 0) ; Black - White
   '(1 0 0) '(0 1 1) ; Red - Cyan 
   '(0 1 0) '(1 0 1) ; Green - Magenta
   '(0 0 1) '(1 1 0))) ; Blue - Yellow

(: cube-edge-midpoints (Listof Point))
(define cube-edge-midpoints
  (list
   '(0 0.5 0) '(1 0.5 1)
   '(0 0 0.5) '(1 1 0.5)
   '(0.5 0 1) '(0.5 1 0)
   '(0 0.5 1) '(1 0.5 0)
   '(0 1 0.5) '(1 0 0.5)))

(: scale+offset : (-> Point Real Point Point))
(define (scale+offset point scale offset) 
  (list 
   (+ (* scale (car point)) (car offset))
   (+ (* scale (cadr point)) (cadr offset))
   (+ (* scale (caddr point)) (caddr offset))))
(module+ test
  (check-equal? (scale+offset '(5 4 2) 3 '(1 2 3)) '(16 14 9)))

(: pattern-to-points : (-> (Listof Point) Point Real (Sequenceof Point)))
(define (pattern-to-points pattern offset scale)
  (stream-map (λ ([point : Point]) (scale+offset point scale offset)) pattern))

(: midpoint : (-> Point Point Point))
(define (midpoint a b)
  (list
   (* 0.5 (+ (car a) (car b)))
   (* 0.5 (+ (cadr a) (cadr b)))
   (* 0.5 (+ (caddr a) (caddr b)))))

;; Creates a stream which takes from the provided streams round-robin style.
;; Assumes the streams are infinite
(: stream-merge-round-robin : (-> (Sequenceof (Sequenceof Point)) (Sequenceof Point)))
(define (stream-merge-round-robin streams)
    (stream-append
        (stream-map (λ ([s : (Sequenceof Point)]) (stream-first s)) streams)
        (stream-filter (lambda ([x : Point]) (not (equal? '(-1 -1 -1) x)))         
            (stream-cons '(-1 -1 -1) ; Need this hack to make it actually lazy
            (stream-merge-round-robin (stream-map (λ ([s : (Sequenceof Point)]) (stream-rest s)) streams))))))

(: point-generator : (-> Point Real (Sequenceof Point)))
(define (point-generator start scale)
  (define center (midpoint start (scale+offset start scale '(0 0 0))))
  (define base-points
    (stream-append
     (pattern-to-points cube-faces start scale)
     (stream center)
     (pattern-to-points cube-edge-midpoints start scale)))
  (define sub-generators (stream-map
     (λ ([pt : Point]) (point-generator pt (* 0.5 scale)))
     (pattern-to-points cube-vertices start (* 0.5 scale))))
  (stream-append base-points (stream-merge-round-robin sub-generators)))

;; A special sequence of points in a unit cube that is designed loosely around the goal of spacing the points as far away from their nearest neighbor as possible, while being agnostic to the total number of points that will be produced
(: points-in-cube : (Sequenceof Point))
(define points-in-cube
    (stream-append 
        cube-vertices 
        (point-generator '(0 0 0) 1)))

;; Generates a color based on a point on the unit cube, with an adjustment for human perception
(: unit-cube->rgb : (-> Point Color))
(define (unit-cube->rgb pt)
    (: pct->byte : (-> Real Byte))
    (define (pct->byte pct) (cast (inexact->exact (round (* 255 pct))) Byte))
    (define red (pct->byte (car pt)))
    (define green (pct->byte (cast (expt (cadr pt) 1.25) Real))) ;; Adjust to make green less dominant in non-green colors (pt is <1, so this is effectively an anti-progressive penalty on greenness)
    (define blue (pct->byte (caddr pt)))
    (make-color red green blue))

;; An infinite deterministic sequence of colors that are reasonably distinct
(: color-gen : (Sequenceof Color))
(define color-gen
    (stream-map unit-cube->rgb points-in-cube))
    
(module+ test
    (println "These should be distinct colors: ")
    (apply beside 
      (cast 
        (stream->list (stream-take (stream-map 
              (lambda ([c : Color]) 
                (overlay 
                  (square 10 "solid" c ) 
                  (square 12 "solid" "white"))) 
          color-gen)
          500)) 
      (List* Image Image (Listof Image)))))

(provide color-gen)

