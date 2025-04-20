# CA - DSL

Authors: Nate White and Jordan Zedeck

`CA-DSL` is a Domain Specific Language for expressing and simulating cellular automata in Typed Racket. In `CA-DSL`, [Conway's Came of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) can easily be expressed as the following:

``` racket
(define conways : LifelikeRule
    (lifelike 
        [born 3]
        [survive 2 3]))
```

This example uses the `lifelike` macro, which can be used to specify transition rules of cellular automata which can be considered ["life-like"](https://conwaylife.com/wiki/Life-like_cellular_automaton). Life-like transition rules define the behavior of 2 dimensional cellular automata which contain cells that are either `alive` or `dead`. The `survive` clause describes the allowed numbers of `alive` neighbors required for an `alive` cell to remain `alive`, where the the [Moore Neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood) is used as the definition for neighorbing cells. The `born` clause is used to describe the allowed number of `alive` neighbors a `dead` cell is required to have to become `alive`. In order to run and visualize Conway's Game of Life, or any other Rule description, we create a starting World, which describes the layout of Cells and States, and a Renderer. These, along with the Rule, are passed to the `run` function.

``` racket
(define-2d-world world : AliveOrDead 
    #:state-map (rect-from 50 50
                (biased-random-select 
                    (list (cons alive 3) (cons dead 4)))))

(define renderer : LifelikeRenderer (make-2d-renderer colormap-alive-or-dead))
(run world conways renderer)
```

This example uses a World with a random configuration of states which are `alive` or `dead` in a square with a width of 50 cells. 



