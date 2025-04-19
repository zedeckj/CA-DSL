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
  @deftype[AliveOrDead]
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
  Represents the construct which enumerates the cells and their current states.
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
  A function type @racket[(C -> Boolean)] that determines if a cell should be updated by the rule. Cells that don't pass this filter keep their original state for the entire simulation.
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
  Main entry point to a program in this language. Opens a native window to visualize the cellular automata. This function takes a world, a rule
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
    @item{@verbatim{count in state} - Tests if exactly @racket[count] neighbors are in @racket[state]}
    @item{@verbatim{(count₁ count₂ ...) in state} - Tests if the number of neighbors in @racket[state] matches any of the counts}
    @item{@verbatim{all in state} - Tests if all neighbors are in @racket[state]}
    @item{@verbatim{some in state} - Tests if at least one neighbor is in @racket[state]}
    @item{Boolean operators: @verbatim[and], @verbatim[or], @verbatim[not], @verbatim[nand], @verbatim[implies], @verbatim[xor], @verbatim[nor]}
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

@section{Renderer}

This section documents the functions and constants exported by the renderer module, which provides visualization capabilities for cellular automata.

@defproc[(make-2d-renderer [color-map (ColorMap S)]
                          [#:cell-width-px cell-width-px Positive-Integer 25]
                          [#:origin-px origin-px Posn (Posn 0 0)])
         (Renderer Posn O S)]{
  Creates a renderer function that can translate a world where cells are represented by @racket[Posn]s to a graphical representation.
  
  The parameters are:
  @itemlist[
    @item{@racket[color-map] - A function that maps states to colors for visualization}
    @item{@racket[#:cell-width-px] - The width of each cell in pixels (default: 25)}
    @item{@racket[#:origin-px] - The position of the top-left corner of the viewport in the world coordinate system (default: (0,0))}
  ]
  
  The returned function has type @racket[(World Posn O S) -> Image], which renders the current state of the world to an image.
  
  The renderer:
  @itemlist[
    @item{Shows cells with states as colored squares based on the @racket[color-map]}
    @item{Shows cells not in the state map as gray squares with an "x"}
    @item{Draws a grid of cell outlines in black}
    @item{Crops the view to show only what fits within the window dimensions}
  ]
}

@section{Colormaps}

This section documents the functions and constants exported by the colormaps module, which provides color mapping capabilities for visualizing cellular automata states.

@subsection{Predefined Colors}

@deftogether[(
  @defthing[BLACK Color]
  @defthing[WHITE Color]
  @defthing[RED Color]
  @defthing[GREEN Color]
  @defthing[BLUE Color]
  @defthing[YELLOW Color]
  @defthing[PURPLE Color]
  @defthing[PINK Color]
  @defthing[ORANGE Color]
  @defthing[GRAY Color]
  @defthing[TRANSPARENT Color]
)]{
  Predefined color constants for use in colormaps and visualization.
}

@defthing[COLOR_LIST (Listof Color)]{
  A list of predefined colors used by the @racket[make-default-colormap] function.
  The list contains: WHITE, BLACK, RED, GREEN, BLUE, YELLOW, PURPLE, PINK, and ORANGE.
}

@subsection{Colormap Functions}

@defproc[(colormap-alive-or-dead [state AliveOrDead]) Color]{
  A predefined colormap for @racket[AliveOrDead] states.
  Maps @racket['alive] to @racket[BLACK] and @racket['dead] to @racket[WHITE].
}

@defproc[(make-default-colormap [states (Listof S)]) (ColorMap S)]{
  Creates a colormap that assigns a distinct color to each state in the provided list.
  
  The function:
  @itemlist[
    @item{Maps the first state to the first color in @racket[COLOR_LIST], the second state to the second color, and so on}
    @item{Raises an error if there are more states than available colors in @racket[COLOR_LIST]}
    @item{Raises an error if asked to map a state not in the original list}
  ]
}

@defproc[(make-grayscale-colormap [minimum Integer] [maximum Integer]) (ColorMap Integer)]{
  Creates a colormap that maps integers to grayscale colors, linearly interpolating between black and white.
  
  The parameters:
  @itemlist[
    @item{@racket[minimum] - Maps to black (RGB: 0,0,0)}
    @item{@racket[maximum] - Maps to white (RGB: 255,255,255)}
  ]
  
  Values between @racket[minimum] and @racket[maximum] are mapped to proportional shades of gray.
  Values less than @racket[minimum] are clamped to black, and values greater than @racket[maximum] are clamped to white.
}

@section{Neighborhoods}

This section documents the functions exported by the neighborhoods module, which provides predefined neighborhood patterns for cellular automata.

@defproc[(moore-neighborhood-outline [distance Positive-Integer]) (Neighborhood Posn)]{
  Creates a set of positions that form the outline of a Moore neighborhood at the specified distance from the center.
  
  The function returns a set containing positions that are exactly @racket[distance] units away from the center
  (in a square pattern). This includes all positions along the perimeter of a square with side length @racket[2 * distance + 1].
  
  The parameter:
  @itemlist[
    @item{@racket[distance] - The distance from the center to the outline positions}
  ]
  
  For example, @racket[(moore-neighborhood-outline 1)] returns a set containing the 8 positions that are exactly 1 unit
  away from the center, which forms the standard Moore neighborhood used in cellular automata like Conway's Game of Life.
}

@defproc[(moore-neighborhood [distance Positive-Integer 1]) (Neighborhood Posn)]{
  Creates a complete Moore neighborhood with the specified radius.
  
  The function returns a set containing all positions within @racket[distance] units from the center
  (in a square pattern, using the Chebyshev distance).
  
  The parameter:
  @itemlist[
    @item{@racket[distance] - The maximum distance from the center (default: 1)}
  ]
  
  For example:
  @itemlist[
    @item{@racket[(moore-neighborhood)] returns the standard 8-cell Moore neighborhood}
    @item{@racket[(moore-neighborhood 2)] returns a 24-cell extended Moore neighborhood}
  ]
  
  The function works by unioning the outlines at each layer from 1 to @racket[distance].
}