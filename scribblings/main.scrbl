#lang scribble/manual
@(require scribble/base "utils.rkt"
          (for-label ca-dsl "../main.rkt" racket 2htdp/universe))

@title{CA-DSL: A DSL for designing and visualizing cellular automata}

@defmodule[ca-dsl]

@section{Types}

@subsection{Posn}

@defstruct*[posn ([x Integer] [y Integer]) #:transparent]{
  Represents a cell's position on a 2D coordinate plane.
}

@defproc[(posn-add [posn1 Posn] [posn2 Posn]) Posn]{
  Component-wise addition of two positions.
}

@defproc[(posn-scale [n Integer] [posn Posn]) Posn]{
  Produces a new Posn by multiplying a scalar to a Posn's coordinates.
}

@subsection{States}

@defform[(define-states states-name : type (state-val ...))]{
  A macro for defining enumerated states for cellular automata. This defines:
  
  @itemlist[
    @item{A type named @racket[type] representing the union of all states}
    @item{A list @racket[states-name] containing all states}
    @item{Individual bindings for each @racket[state-val]}
  ]
}

The library comes with a predefined state type:

@deftogether[(
  @defidform[AliveOrDead]
  @defthing[alive AliveOrDead]
  @defthing[dead AliveOrDead]
)]{
  A predefined state type representing the binary states commonly used in cellular automata like Conway's Game of Life.
}

@subsection{Core Types}

The library uses parametric types with a conventional meaning for the following type variables:
@itemlist[
  @item{@racket[C]: The type of cells (e.g., @racket[Posn] for cartesian grids)}
  @item{@racket[O]: The type of offsets (e.g., @racket[Posn] for cartesian neighborhoods)}
  @item{@racket[S]: The type of states (e.g., @racket[AliveOrDead])}
]

@deftypeconstr[(StateMap C S)]{
  Represents a mutable hash table mapping cells to their states.
}

@deftypeconstr[(Topology C O)]{
  Represents transitions between cells using offsets. A function type @racket[(C O -> (Union C Void))] that takes a cell and an offset and returns either a new cell or void (for invalid transitions).
}

@deftypeconstr[(Neighborhood O)]{
  Represents a set of offsets used for transition calculations.
}

@deftypeconstr[(Rule C O S)]{
  A function type @racket[((StateMap C S) (Topology C O) C -> S)] that computes the new state for a cell based on the current state map, topology, and the cell itself.
}

@deftypeconstr[(ActiveFilter C)]{
  A function type @racket[(C -> Boolean)] that determines if a cell should be updated by the rule. Cells that don't pass this filter keep their original state.
}

@deftypeconstr[(ColorMap S)]{
  A function type @racket[(S -> Color)] that maps states to colors for visualization.
}

@defstruct*[(world C O S) ([state-map (StateMap C S)]
                           [topology (Topology C O)]
                           [active-filter (ActiveFilter C)]) #:transparent]{
  A structure representing all characteristics of a simulated "World" in which a Rule can be applied.
}

@deftypeconstr[(Renderer C O S)]{
  A function type @racket[((World C O S) -> Image)] that transforms a World into an Image for visualization.
}

@subsection{Specialized Types}

The library provides specialized types for common cellular automata patterns:

@deftypeconstr[(2DWorld S)]
@deftypeconstr[(2DRenderer S)]
Specialized types for 2D worlds where cells and offsets are both represented by @racket[Posn].

@deftype[LifelikeRule]
@deftype[LifelikeWorld]
@deftype[LifelikeRenderer]
Specialized types for cellular automata similar to Conway's Game of Life, using the binary @racket[AliveOrDead] state on a 2D world.

@section{Run}

@defproc[(run [world (World C O S)]
              [rule (Rule C O S)]
              [renderer (Renderer C O S)])
         (World C O S)]{
  Opens a native window to visualize the cellular automata. This function takes a world, a rule
  for state transitions, and a renderer for visualization, then displays the cellular automata
  simulation in a window. When the simulation completes, it returns the final world state.
         }

@section{Rule}

This section documents the macros exported by the rule module, which allow for defining cellular automata rules in various formats.

@defform[(rule #:cell-type cell-type
               #:offset-type offset-type
               #:state-type state-type
               #:neighborhood neighborhood
               clause ...)]{
  Creates a rule function for a cellular automaton. The resulting function has the type 
  @racket[(StateMap cell-type state-type) (Topology cell-type offset-type) cell-type -> state-type], 
  which computes the new state for a cell based on its neighborhood.
  
  The parameters are:
  @itemlist[
    @item{@racket[#:cell-type] - The type used to represent cells in the grid}
    @item{@racket[#:offset-type] - The type used to represent offsets within the neighborhood}
    @item{@racket[#:state-type] - The type used to represent cell states}
    @item{@racket[#:neighborhood] - An expression that evaluates to a @racket[Neighborhood] of offsets}
    @item{@racket[clause ...] - One or more transition rules that determine state changes}
  ]
  
  Each @racket[clause] has the form @racket[(from-state -> to-state) condition ...] where:
  @itemlist[
    @item{@racket[from-state] is the initial state}
    @item{@racket[to-state] is the resulting state}
    @item{@racket[condition ...] are optional conditions that must be satisfied for the transition to occur}
  ]
  
  Conditions can include:
  @itemlist[
    @item{@racket[count in state] - Tests if exactly @racket[count] neighbors are in @racket[state]}
    @item{@racket[(count₁ count₂ ...) in state] - Tests if the number of neighbors in @racket[state] matches any of the counts}
    @item{@racket[all in state] - Tests if all neighbors are in @racket[state]}
    @item{@racket[some in state] - Tests if at least one neighbor is in @racket[state]}
    @item{Boolean operators: @racket[and], @racket[or], @racket[not], @racket[nand], @racket[implies], @racket[xor], @racket[nor]}
  ]
}

@defform[(moore-rule #:state-type state-type
                    clause ...)]{
  A convenience macro that expands to @racket[rule] with @racket[Posn] as both the cell and offset types,
  and uses a Moore neighborhood with radius 1. This is commonly used for 2D cellular automata like Conway's Game of Life.
  
  The parameters are:
  @itemlist[
    @item{@racket[#:state-type] - The type used to represent cell states}
    @item{@racket[clause ...] - One or more transition rules as in @racket[rule]}
  ]
}

@defform[(lifelike [(born condition ...)]
                  [(survive condition ...)])]{
  A specialized macro for creating rules similar to Conway's Game of Life, using the @racket[AliveOrDead] state type.
  It automatically creates a rule where:
  
  @itemlist[
    @item{Dead cells become alive if they satisfy the @racket[born] conditions}
    @item{Living cells stay alive if they satisfy the @racket[survive] conditions}
    @item{All other cells become or stay dead}
  ]
  
  This provides a concise way to define various "Life-like" cellular automata such as Conway's Game of Life,
  HighLife, Day & Night, and others.
}