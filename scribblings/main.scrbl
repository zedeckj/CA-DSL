#lang scribble/manual
@(require scribble/base "utils.rkt"
          (for-label ca-dsl racket 2htdp/universe))

@title{CA-DSL: A DSL for designing and visualizing cellular automata}

@link["https://conwaylife.com/wiki/"]{Cellular Automata} are a beautiful demonstration of how complexity can emerge from simple rules. This library helps visualize and explore them. 

@defmodule[ca-dsl]

@section{Types}

@subsection{Posn}

@defstruct*[Posn ([x Integer] [y Integer]) #:transparent]{
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

@defstruct*[(World C O S) ([state-map (StateMap C S)]
                           [topology (Topology C O)]
                           [active-filter (ActiveFilter C)]) 
                           #:transparent]{
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
  
  In the general case, @racket[clause] has the form @racket[(from-state -> to-state) condition ...] where:
  @itemlist[
    @item{@racket[from-state] is the initial state}
    @item{@racket[to-state] is the resulting state}
    @item{@racket[condition ...] are optional conditions that must be satisfied for the transition to occur}
  ]

  Also supported is chaining multiple state transitions controlled by the same condition using @racket[(state1->state2->state3 ...) condition ...] as well as specifying any state using _.  
  
  Conditions can include:
  @itemlist[
    @item{@verbatim{count in state} - Tests if exactly @racket[count] neighbors are in @racket[state]}
    @item{@verbatim{(count₁ count₂ ...) in state} - Tests if the number of neighbors in @racket[state] matches any of the counts}
    @item{@verbatim{all in state} - Tests if all neighbors are in @racket[state]}
    @item{@verbatim{some in state} - Tests if at least one neighbor is in @racket[state]}
    @item{Boolean operators to join anything above in order of highest to lowest precedence: not, and/nand, xor, or/nor, implies }
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
  A list of predefined colors containing @racket{WHITE, BLACK, RED, GREEN, BLUE, YELLOW, PURPLE, PINK, and ORANGE.}
}

@subsection{Colormap Functions}

@defproc[(colormap-alive-or-dead [state AliveOrDead]) Color]{
  A predefined colormap for @racket[AliveOrDead] states.
  Maps @racket['alive] to @racket[BLACK] and @racket['dead] to @racket[WHITE].
}

@defproc[(make-default-colormap [states (Listof S)]) (ColorMap S)]{
  Creates a colormap that assigns a distinct color to each state in the provided list. Deterministic and can support any number of arguments.
  
  Raises an error if asked to map a state not in the original list.
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

@section{Grid Utilities}

This library provides utilities for creating and manipulating grid-based cellular automata and other grid-based simulations.


@defproc[(rect-custom [width integer?] 
                     [height integer?]
                     [state-fn (-> posn? any/c)])
                     hash?]{
  Creates a statemap over a rectangular region with cells initialized using the given function.
  
  The function @racket[state-fn] is called with each position in the rectangle
  to determine the state for that cell.
}

@defproc[(rect-from [width integer?] 
                   [height integer?]
                   [state-generator (-> any/c)])
                   hash?]{
  Creates a statemap over a rectangular region with cells initialized using the given thunk.
  
  The function @racket[state-generator] is called for each cell to generate its state.
}

@defproc[(rect-solid [width integer?] 
                    [height integer?]
                    [state any/c])
                    hash?]{
  Creates a statemap of a rectangle composed of the given state.
  
  All cells in the rectangle will have the same @racket[state] value.
}

@defproc[(overlay/statemaps [topology any/c]
                           [offset any/c]
                           [statemap hash?] ...)
                           hash?]{
  Combines multiple statemaps by overlaying them at specified positions.
  
  Arguments alternate between an absolute position and then a statemap to be placed 
  with its lower left corner at that absolute position. Similar in functionality to 
  @racket[overlay/xy] in the htdp2/image library.
}

@defproc[(biased-random-select [weighted-sequence (listof (cons/c any/c exact-nonnegative-integer?))])
                              (-> any/c)]{
  Creates a function that returns random selections from a weighted list of values.
  
  @racket[weighted-sequence] is a list of pairs, where each pair consists of a value 
  and its corresponding weight (a non-negative integer). The returned function will 
  select values with probability proportional to their weights.
}

@defform[(path : type-id (x-expr y-expr) segment ...)
         #:grammar [(segment (state-expr magnitude-expr direction state-expr ...))
                    (direction up down left right)]]{
  Creates a statemap representing a path with the specified segments.
  
  @itemlist[
    @item{@racket[type-id] - The type name for the states in the path}
    @item{@racket[x-expr y-expr] - The starting position of the path}
    @item{@racket[state-expr] - The state for the current segment}
    @item{@racket[magnitude-expr] - The length of the current segment}
    @item{@racket[direction] - The direction of the current segment (up, down, left, or right)}
  ]
  
  Multiple segments can be chained together to create complex paths.

  @racket[(path : AliveOrDead (-2 3) ('dead 3 up 5 right 3 down 1 left))]

}

@section{Grid Topologies}

This library provides functions for creating and manipulating different grid topologies for cellular automata and other grid-based simulations.

@defproc[(cartesian-topology [pos posn?] [offset posn?]) posn?]{
  A standard topology which adds Posns linearly.
  
  Returns a new position that is the sum of the input position and offset.
}

@defproc[(truncate-topology [topology (-> any/c any/c (or/c any/c void?))]
                           [predicate (-> any/c any/c any/c boolean?)])
                           (-> any/c any/c (or/c any/c void?))]{
  Restricts a Topology to produce additional Void returns if an input cell, offset, and the 
  output of the original topology do not satisfy the provided predicate.
  
  Returns a new topology function that respects the given predicate.
}

@defproc[(modify-topology [topology (-> any/c any/c (or/c any/c void?))]
                         [modifier (-> any/c (or/c any/c void?))] ...)
                         (-> any/c any/c (or/c any/c void?))]{
  Modifies a Topology with a series of "modifiers", which each take in a Cell and produce either
  a Void or a new Cell value, which are applied in sequence to outputs of a topology.
  
  Returns a new topology function with the modifiers applied.
}

@defproc[(make-finite-cartesian-topology [max-x exact-positive-integer?]
                                        [max-y exact-positive-integer?])
                                        (-> posn? posn? (or/c posn? void?))]{
  Creates a modified cartesian topology which is restricted by the given max-x and max-y values.
  
  Outputs cells from the cartesian topology which have x values or y values with absolute values 
  greater than the given max-x or max-y are turned to Void.
}

@defproc[(in-cartesian-region [point posn?]
                             [max-point posn?]
                             [#:origin origin posn? (posn 0 0)])
                             boolean?]{
  Returns if a point is in a region bounded by the origin and the provided max-point.
  
  A point is in the region if its coordinates are both greater than or equal to the origin's coordinates
  and less than or equal to the max-point's coordinates.
}

@defproc[(make-wrapping-cartesian-topology [x-min integer?]
                                          [x-max integer?]
                                          [y-min integer?]
                                          [y-max integer?])
                                          (-> posn? posn? posn?)]{
  Creates a modified cartesian topology in which Posns outputs are "wrapped" around at the given values.
  
  In this topology, if a coordinate exceeds the maximum value, it wraps around to the minimum value,
  and vice versa.
}

@defproc[(init-2d-world [max-x exact-positive-integer?]
                       [max-y exact-positive-integer?]
                       [state-initializer (-> posn? any/c)])
                       any/c]{
  Convenience function for creating 2D worlds with bounded cartesian topologies.
  
  The state-initializer function is used to set the starting State of each Cell in the StateMap of
  the world.
}

@defform[(define-2d-world world : state-type 
                         #:state-map statemap-expr
                         #:active-filter active-filter-expr
                         #:topology topology-expr)
         #:contracts ([statemap-expr hash?]
                      [active-filter-expr (-> posn? boolean?)]
                      [topology-expr (-> posn? posn? (or/c posn? void?))])]{
  Defines a 2D world with the given parameters.
  
  @itemlist[
    @item{@racket[world] - The identifier to bind the new world to}
    @item{@racket[state-type] - The type of state values in the world}
    @item{@racket[statemap-expr] - Expression producing the state map for the world}
    @item{@racket[active-filter-expr] - Optional expression producing an active filter function}
    @item{@racket[topology-expr] - Optional expression producing a topology function}
  ]
  
  The @racket[#:active-filter] and @racket[#:topology] parameters are optional.
}