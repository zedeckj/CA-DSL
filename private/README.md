# Internal Program Details

## Type Checker problems and workarounds

Types in typed/racket cannot be assigned to variables or passed to functions. This means that in order to work with types, and annotate values such that the type checker is satisfied and able to resolve parametric types, we had to use macros to avoid excessive boilerplate for common pairings of types. For example, removing the boilerplate of specifying Posn as the type of Cell and Offset, but keeping the ability to specify the type of the state in the moore-rule macro would have been possible as a function if types were available at runtime. Furthermore, we also have a large amount of annotations to narrow types, particularly with regards to the parametric polymorphism present in our code. This meant that in our parse chain for the rule macro, we needed to drill down the types so that they would be available for splicing as annotations. Otherwise, the top level run function would fail to resolve a solution to the polymorphic types.

A further problem we encountered with types is their interaction with modules. Types are not able to exported with (provide all-defined-out) which means they must be manually specified as exported from our central types.rkt file. That file was itself a coerced design decision, to ensure that types would be accessible to both parties of modules which may import/export from each other, but need access to the types in order to specify their interface. Circularity in racket imports is determined at the module, not definition level. Thus, we draw an unfortunate analogy to C header files and declare the types separate from the code that uses them. 

## Entry point and compilation

The main entry point to the program is the run function. This takes in a world, a rule, and a renderer. The world consists of the initial statemap as well as the topology (how cells are connected, which is needed to determine the neighborhood used for the rule), an active filter to optionally make certain cells exempt from  the rule, and a mapping of cells to their initial state. The renderer describes how the world should be displayed as an image. The rule describes how the statemap should change at each time tick. The run function will pop up a native window which progresses through the simulation. 

There are two interesting groups of macros in this library: the `rule` macro for defining rules, and the `path` macro which helps with defining wire-like statemaps

### Rule macro

A [rule](rule.rkt) describes how a cell should transition on each time step. The `lifelike` macro compiles to a `moore-rule` macro, which in turn compiles to a `rule` macro, which is the lowest level exposed as surface syntax. The `rule` macro compiles to a lambda that takes in the statemap of the whole simulation, topology, and the particular cell for which it should return the new state of. Its surface syntax is similar to a match statement -- there are branches which specify a rule, which consists of a transition (for example, 'alive -> 'dead ) and optionally a condition which must be met for this transition to apply. 

Example expansions are present in [rule.rkt](rule.rkt). At a high level, the transition is desugared to change any chained transitions (a -> b -> c) into two "clauses" with transitions (a->b) and (b->c) and both having the original condition if present. The condition is parsed into a racket expression by recursively matching datums corresponding to boolean operators according to their precedence, and then evaluating `in` statements by translating them into calls to the `has-neighbors-in-state` runtime function. From there, the `compose-clause` macro interprets the condition and the transition of the clauses into a nested if expression.

#### Example expansion
```racket 
#;(rule
   #:cell-type Posn
   #:offset-type Posn
   #:state-type PredatorsAndPreyState
   #:neighborhood (moore-neighborhood)
   [(empty -> prey) 3 in prey and 0 in predator]
   [(prey -> predator) all in prey]
   [(prey -> prey) 0 in predator]
   [(empty -> predator) 2 in predator and some in prey]
   [(predator -> predator) some in prey]
   [(_ -> empty)])

;; EXPANDS TO

#;(lambda ([state-map : (StateMap Posn PredatorsAndPreyState)] [topology : (Topology Posn Posn)] [cell : Posn])
    (let ([in-state : PredatorsAndPreyState (hash-ref state-map cell)]
          [neighbors : (Listof PredatorsAndPreyState) (get-neighbors cell state-map topology (moore-neighborhood))])
      (if (and (eq? in-state (ann empty PredatorsAndPreyState))
               (and (has-neighbors-in-state? (ann prey PredatorsAndPreyState) neighbors (list (ann 3 Nonnegative-Integer)))
                    (has-neighbors-in-state? (ann predator PredatorsAndPreyState) neighbors (list (ann 0 Nonnegative-Integer)))))
          (ann prey PredatorsAndPreyState)
          (if (and (eq? in-state (ann prey PredatorsAndPreyState))
                   (has-neighbors-in-state? (ann prey PredatorsAndPreyState) neighbors (list (length neighbors))))
              (ann predator PredatorsAndPreyState)
              (if (and (eq? in-state (ann prey PredatorsAndPreyState))
                       (has-neighbors-in-state? (ann predator PredatorsAndPreyState) neighbors (list (ann 0 Nonnegative-Integer))))
                  (ann prey PredatorsAndPreyState)
                  (if (and (eq? in-state (ann empty PredatorsAndPreyState))
                           (and (has-neighbors-in-state? (ann predator PredatorsAndPreyState) neighbors (list (ann 2 Nonnegative-Integer)))
                                (has-neighbors-in-state?
                                 (ann prey PredatorsAndPreyState)
                                 neighbors
                                 (range 1 (add1 (set-count (moore-neighborhood)))))))
                      (ann predator PredatorsAndPreyState)
                      (if (and (eq? in-state (ann predator PredatorsAndPreyState))
                               (has-neighbors-in-state?
                                (ann prey PredatorsAndPreyState)
                                neighbors
                                (range 1 (add1 (set-count (moore-neighborhood))))))
                          (ann predator PredatorsAndPreyState)
                          (if #t (ann empty PredatorsAndPreyState) (error (format "No valid transition from state ~a" in-state))))))))))
```

### Path macro
The [path macro](statemaps.rkt) is intended to make it easy to express the initial state of worlds like [wireworld](../demos/wireworld.rkt). It works by creating a series of width-1 rectangles of a specified state to create a statemap that can then be overlayed onto another statemap. The macro makes it easier to do this by allowing chaining such that the end of one rectangle becomes the start of the next.

#### Example expansion
```racket
#|
(path (1 0) ['state1 5 up 3 right] ['state2 4 down 1 left])
->
(path-internal (Posn 1 0)
        (list
        (list 'state1 5 'up) (list 'state1 3 'right))
            (list 'state2 4 'down) (list 'state2 1 left) '())
```