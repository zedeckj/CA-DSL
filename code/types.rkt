#lang typed/racket

(require typed/2htdp/image)

;; Represents a cell on a 2D coordinate plane
(define-struct posn 
    ([x : Integer] 
    [y : Integer]) #:type-name Posn #:transparent)
#| 
Meaning for parametric type variables: 
    C represents the type of the cell. 
        Example: Posns might be used to identify cells in a cartesian or hexadecimal topology.
    O represents the type of the offsets. 
        Example: Posn in a cartesian grid, or cardinal directions if the neighborhood is limited to only the immediately surrounding cells.
    S represents the type of the states. 
        Example: (U 'alive 'dead), Byte
|#
(define-type AliveOrDead (U 'alive 'dead))

;;  Represents a data structure for storing the states of cells in the CA
(define-type (StateMap C S) (Mutable-HashTable C S))

;; Represents a transition from cell and an offset to a new cell.
(define-type (Topology C O) (C O -> (Union C Void)))

;; Represents a function which takes in a cell and returns true if its state should be changed by the rule, otherwise the cell stays in its starting state for the duration of the simulation.
(define-type (ActiveFilter C) (C -> Boolean))

;; Represents what color should be used to represent a state
(define-type (ColorMap S) (S -> Color))
    
;; A function which, when given a StateMap and Topology, creates a new StateMap representing a time stepped instance of the World's StateMap
#; (define-type (Rule C O S) ((StateMap C S) (Topology C O) -> (StateMap C S)))

;; A function which, when given a StateMap, Topology, and Cell, returns the new State for that Cell
(define-type (Rule C O S) ((StateMap C S) (Topology C O) C -> S))

(define-type (Neighborhood O) (Setof O))

(define-struct (C O S) world
    ([state-map : (StateMap C S)] 
    [topology : (Topology C O)] 
    [active-filter : (ActiveFilter C)]) #:type-name World)

(define-type (Renderer C O S) ((World C O S) -> Image))

(define-type LifelikeRule (Rule Posn Posn AliveOrDead))
(define-type LifelikeWorld (World Posn Posn AliveOrDead))
(define-type LifelikeRenderer (Renderer Posn Posn AliveOrDead))

; TODO Is there a provide-all-out for types?
(provide StateMap Topology ActiveFilter ColorMap Rule Renderer AliveOrDead World (struct-out world) (struct-out posn) Neighborhood Posn LifelikeRule LifelikeWorld LifelikeRenderer)
