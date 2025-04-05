1. Fix type checking all arguments in rule macro
2. Topology and active filter composition rewrite for speed 
3. Figuring out static check of possible neighborhood size
4. non totalistic features in rule macro
5. more topology stuffs in library for variations 
6. weirder example programs 

Rule Changes:
- Now a function from World Cell -> State, called for each cell that is active in the state map
- Can now mutate the current state map instead of creating a new state map
- No need to post hoc enforce active filter, only call rule for cells that are active 



4/4/25
- Implement `wire` macro, and run wireworld with preset 
- Static check for neighbor counts in `lifelike` and `moore-neighborhood`, with option to specify when using `rule`
- Logical operators `and`, `or`, `not`
- Neighbor count negation `except`
- Arbitrary function call syntax `fn`

