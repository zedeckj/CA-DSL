# Internal Program Details

## Type Checker problems and workarounds

Types in typed/racket cannot be assigned to variables or passed to functions. This means that in order to work with types, and annotate values such that the type checker is satisfied and able to resolve parametric types, we had to use macros to avoid excessive boilerplate for common pairings of types. For example, removing the boilerplate of specifying Posn as the type of Cell and Offset, but keeping the ability to specify the type of the state in the moore-rule macro would have been possible as a function if types were available at runtime. Furthermore, we also have a large amount of annotations to narrow types, particularly with regards to the parametric polymorphism present in our code. This meant that in our parse chain for the rule macro, we needed to drill down the types so that they would be available for splicing as annotations. Otherwise, the top level run function would fail to resolve a solution to the polymorphic types.

A further problem we encountered with types is their interaction with modules. Types are not able to exported with (provide all-defined-out) which means they must be manually specified as exported from our central types.rkt file. That file was itself a coerced design decision, to ensure that types would be accessible to both parties of modules which may import/export from each other, but need access to the types in order to specify their interface. Circularity in racket imports is determined at the module, not definition level. Thus, we draw an unfortunate analogy to C header files and declare the types separate from the code that uses them. 

## Entry point and compilation

The main entry point to the program is the run function. This takes in a world, a rule, and a renderer. The user level documentation specifies how these can be constructed easily. The world and renderer are just functions to create the corresponding types specified in types.rkt. The rule is a more involved macro which requires further explanation. 

### Rules

A [rule](rule.rkt) describes how a cell should transition on each time step. The `lifelike` macro compiles to a `moore-rule` macro, which in turn compiles to a `rule` macro, which is the lowest level exposed as surface syntax. The `rule` macro compiles to a lambda that takes in the statemap of the whole simulation, topology, and the particular cell for which it should return the new state of. Its surface syntax is similar to a match statement -- there are branches which specify a rule, which consists of a transition (for example, 'alive -> 'dead ) and optionally a condition which must be met for this transition to apply. 

#### Transition
Two shorthands for specifying a transition are implemented: Specifying  ('alive -> 'dead). Two 

### Example annotated expansion