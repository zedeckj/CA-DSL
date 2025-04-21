```
<RULE> ::= 
        (rule 
                #:neighborhood <expr>
                #:state-type <type>
                #:cell-type <type>
                #:offset-type <type>
                <BRANCH> ...+ )

			

<BRANCH> ::= [<TRANSITION> <COND>]


<TRANSITION> ::= (<STATE-CHAIN>)
             | (_ -> <STATE>)

<STATE-CHAIN> ::= <STATE> -> <STATE> 
        | <STATE> -> <STATE-CHAIN>

<COND> ::=
        | <COUNTS> in <STATE>
        | <COND> and <COND>
        | <COND> or <COND>
        | not <COND>


<COUNTS> ::= (<natural> <natural> ...)
         | <natural>
         | some
         | all


<STATE> ::= <expr>

<CELL> ::= <expr>

<OFFSET> ::= <expr>
```
