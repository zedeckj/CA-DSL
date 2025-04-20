# CA - DSL

Authors: Nate White and Jordan Zedeck

`CA-DSL` is a Domain Specific Language for expressing and simulating cellular automata in Typed Racket. In `CA-DSL`, [Conway's Came of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) can easily be expressed as the following:

``` racket
(define conways : LifelikeRule
    (lifelike 
        [born 3]
        [survive 2 3]))
```

This example uses the `lifelike` macro, which can be used to specify transition rules of cellular automata which can be considered ["life-like"](https://conwaylife.com/wiki/Life-like_cellular_automaton). Life-like transition rules define the behavior of 2 dimensional cellular automata which contain cells that are either `alive` or `dead`. The `survive` clause describes the allowed numbers of `alive` neighbors required for an `alive` cell to remain `alive`, where the the [Moore Neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood) is used as the definition for neighorbing cells. The `born` clause is used to describe the allowed number of `alive` neighbors a `dead` cell is required to have to become `alive`. In order to run and visualize Conway's Game of Life, or any other `Rule` description, we create a starting `World`, which describes the layout of cells and states, and a `Renderer`. These, along with the `Rule`, are passed to the `run` function.

``` racket
(define-2d-world world : AliveOrDead 
    #:state-map (rect-from 50 50
                (biased-random-select 
                    (list (cons alive 3) (cons dead 4)))))

(define renderer : LifelikeRenderer (make-2d-renderer colormap-alive-or-dead))
(run world conways renderer)
```

This example uses a World with a random configuration of states which are `alive` or `dead` in a square with a width of 50 cells. 

<img src="https://github.com/zedeckj/ca-dsl/blob/d1132788a2340b63099d0a22e31462cfaeab2c24/gfx/conways-small.gif" width="400" height="410"/>


The `lifelike` syntax used in this example is a macro which wraps around `moore-rule`, which itself wraps around `rule`. Conway's Game of Life can also be expressed, more verbosely, with these directly.

``` racket
(define conways : LifelikeRule
    (moore-rule 
        #:state-type AliveOrDead
        [(dead -> alive) 3 in alive]
        [(alive -> alive) (2 3) in alive]
        [(_ -> dead)]))
```

``` racket
(define conways : LifelikeRule
    (rule 
        #:cell-type Posn
        #:offset-type Posn
        #:state-type AliveOrDead
        #:neighborhood (moore-neighborhood)
        [(dead -> alive) 3 in alive]
        [(alive -> alive) (2 3) in alive]
        [(_ -> dead)]))
```

In the `rule` form, one might notice `Posn` being passed as a keyword argument to `#:cell-type` and `#:offset-type`. This indicates that this rule operates on a 2D plane of `Posn`s, which are coordinates. The built-in `make-2d-renderer` is capable of creating `Renderer`s for this type of `World`, but `Rules` with any type can easily be created and ran with a custom `Renderer`. 

### More Complex Rules

#### [Star Wars](https://quuxplusone.github.io/blog/2020/06/29/star-wars-ca/)

``` racket
(define star-wars
    (moore-rule
        #:state-type Integer
        [(0 -> 1) 2 in 1]
        [(1 -> 1) (3 4 5) in 1]
        [(1 -> 2)]
        [(2 -> 3)]
        [(_ -> 0)]))
```

<img src="https://github.com/zedeckj/ca-dsl/blob/5b55b0059abaf65a242d85b916a906eda72f621d/gfx/star-wars2.gif" width="400" height="410"/>




#### [Wireworld](https://en.wikipedia.org/wiki/Wireworld)

``` racket 

(define-states states : WireWorldState (head tail conductor insulator))

(define wireworld
    (moore-rule
        #:state-type WireWorldState
        [(head -> tail -> conductor)]
        [(conductor -> head) (1 2) in head]
        [(conductor -> conductor)]
        [(_ -> insulator)]))
```

<img src="https://github.com/zedeckj/ca-dsl/blob/f4616b13c30c7412e6c578e059fdde3047e0699f/gfx/wireworld.gif" width="400" height="410"/>

#### "Predators and Prey"

``` racket
(define-states states : PredatorsAndPreyState (empty prey predator))

(define predators-and-prey
    (moore-rule
        #:state-type PredatorsAndPreyState
        [(empty -> prey) 3 in prey and 0 in predator]
        [(prey -> predator) all in prey]
        [(prey -> prey) 0 in predator]
        [(empty -> predator) 2 in predator and some in prey]
        [(predator -> predator) some in prey]
        [(_ -> empty)]))
```
<img src="https://github.com/zedeckj/ca-dsl/blob/79d8f9f1428073487195934f08537fdec8b6f21b/gfx/predators-and-prey.gif" width="400" height="410"/>