#lang typed/racket

(require typed/2htdp/image)

;; Represents a cell on a 2D coordinate plane
(define-struct posn 
    ([x : Integer] 
    [y : Integer]) #:type-name Posn #:transparent)

;; Meaning for parametric type variables: 
;; C represents the type of the cell. 
;; O represents the type of the offsets
;; S represents the type of the states 


;;  Represents a data structure for storing the states of cells in the CA
(define-type (StateMap C S) (HashTable C S))
(module+ examples
    (define STATEMAP-EMPTY : (StateMap Any Any) (make-hash ))
    (define STATEMAP-3x3-LIVE-CROSS : (StateMap Posn (U 'alive 'dead)) 
        (hash 
            (Posn 0 0) 'alive (Posn 1 0) 'dead (Posn 2 0) 'alive
            (Posn 1 0) 'dead (Posn 1 1) 'alive (Posn 2 1) 'dead
            (Posn 2 0) 'alive (Posn 1 0) 'dead (Posn 2 0) 'alive 

            (Posn 3 4) 'dead))
)



;; Represents a transition from cell and an offset to a new cell.
(define-type (Topology C O) (C O -> C))

;; Represents a function which takes in a cell and returns true if it should be actively
;; updated by a rule.
(define-type (ActiveFilter C) (C -> Boolean))

;; Represents how to graphically represent a state
(define-type (ColorMap S) (S -> Color))

(module+ examples
    (: color-map-is-alive : ((U 'alive 'dead) -> Color))
    (define (color-map-is-alive state)
        (match state
            ['alive (make-color 50 50 50)]
            ['dead (make-color 200 200 200)]))

    

)
;; A function which, when given a World, creates a new StateMap representing a time stepped
;; instance of the World's StateMap
(define-type (Rule C O S) ((StateMap C S) (Topology C O) -> (StateMap C S)))

(define-struct (C O S) world 
    ([state-map : (StateMap C S)] 
    [topology : (Topology C O)] 
    [active-filter : (ActiveFilter C)]) #:type-name World)


(define-type (Renderer C O S) ((World C O S) -> Image))

(module+ examples 
    (provide (all-defined-out))
)

(provide StateMap Topology ActiveFilter ColorMap Rule Renderer World (struct-out world) (struct-out posn)  Posn);; posn-x posn-y Posn posn?)
