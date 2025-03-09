#lang typed/racket
(: fold-left
   (All (C A B ...)
        (-> 
          (-> C A B ... B C) 
          C 
          (Listof A) 
          (Listof B) ... B
          C)))
(define (fold-left f i as . bss)
  (if (or (null? as)
          (ormap null? bss))
      i
      (apply fold-left
             f
             (apply f i (car as) (map car bss))
             (cdr as)
             (map cdr bss))))