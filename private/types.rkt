#lang typed/racket

(require (for-syntax syntax/parse))
(require typed/2htdp/image)

;; Represents a cell on a 2D coordinate plane
(define-struct posn 
    ([x : Integer] 
    [y : Integer]) #:type-name Posn #:transparent)

;; Component-wise addition of two positions
(: posn-add : Posn Posn -> Posn)
(define (posn-add posn1 posn2)
  (Posn (+ (posn-x posn1) (posn-x posn2))
        (+ (posn-y posn1) (posn-y posn2))))

;; Produces a new Posn by multiplying a Scalar to a Posn's coordinates
(: posn-scale : Integer Posn -> Posn)
(define (posn-scale n posn)
  (Posn (* n (posn-x posn))
        (* n (posn-y posn))))

#| 
Meaning for parametric type variables: 
    C represents the type of the cell. 
        Example: Posns might be used to identify cells in a cartesian or hexadecimal topology.
    O represents the type of the offsets. 
        Example: Posn in a cartesian grid, or cardinal directions if the neighborhood is limited to only the immediately surrounding cells.
    S represents the type of the states. 
        Example: (U 'alive 'dead), Byte
|#

(define-syntax (define-states stx)
    (syntax-parse stx
        [(_ states-name:expr (~datum :) type:id (state-val:id ...+))
        #'(begin
            (define-type type (U (quote state-val) ...))
            (define states-name : (Listof type) (list (quote state-val) ...))
            (define state-val : type (ann (quote state-val) type)) ...)]))


(define-states alive-or-dead : AliveOrDead (alive dead))

;;  Represents a data structure for storing the states of cells in the CA
(define-type (StateMap C S) (Mutable-HashTable C S))

;; Represents a transition from cell and an offset to a new cell.
(define-type (Topology C O) (C O -> (Union C Void)))

;; Represents a function which takes in a cell and returns true if its state should be changed by the rule, otherwise the cell stays in its starting state for the duration of the simulation.
(define-type (ActiveFilter C) (C -> Boolean))

;; Represents what color should be used to represent a state
(define-type (ColorMap S) (S -> Color))


;; A function which, when given a StateMap, Topology, and Cell, returns the new State for that Cell
(define-type (Rule C O S) ((StateMap C S) (Topology C O) C -> S))

;; Represents a set of Offsets which is used in transition condition calculations
(define-type (Neighborhood O) (Setof O))

;; A structure representing all characteristics of a simulated "World" in which a Rule
;; can be applied to 
(define-struct (C O S) world
    ([state-map : (StateMap C S)] 
    [topology : (Topology C O)] 
    [active-filter : (ActiveFilter C)]) #:type-name World)

;; A predefined World for simulations using Posns as the state type and offset type
(define-type (2DWorld S) (World Posn Posn S))
    
;; A function which defines as a transformation from a World to an Image
(define-type (Renderer C O S) ((World C O S) -> Image))

;; A predefined Renderer for 2D Worlds 
(define-type (2DRenderer S) (Renderer Posn Posn S))

;; Note, we use the term "Lifelike" as meaning being similiar to Conway's Game of Life,
;; or a 2 dimensional cellular automata of states that are Alive or Dead, and transition using
;; a radius 1 Moore Neighborhood.

;; A predefined "Likelike" Rule, which uses the binary AliveOrDead state on a 2DWorld.
(define-type LifelikeRule (Rule Posn Posn AliveOrDead))

;; A predefined "Likelike" World, which is a 2DWorld of AliveOrDead states.
(define-type LifelikeWorld (2DWorld AliveOrDead))

;; A predefined "Lifelike" Renderer, which is a 2DRenderer for the AliveOrDead state.
(define-type LifelikeRenderer (2DRenderer AliveOrDead))

; TODO Is there a provide-all-out for types?
(provide StateMap Topology 2DRenderer 2DWorld ActiveFilter ColorMap Rule Renderer 
    AliveOrDead define-states alive dead World (struct-out world) (struct-out posn) Neighborhood Posn 
    LifelikeRule LifelikeWorld LifelikeRenderer posn-add posn-scale)
