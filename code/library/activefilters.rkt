#lang typed/racket

(require "../types.rkt")

;; Deactivates the given set of cells
(: deactivate-cells : (All (C) ((Setof C) -> (ActiveFilter C))))
(define (deactivate-cells frozen-cells)
    (lambda ([cell : C])
        (set-member? frozen-cells cell)))

(provide deactivate-cells)