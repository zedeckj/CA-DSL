```
<RULE> ::= 
        (rule 
                #:neighborhood <expr>
                #:state-type <type>
                #:cell-type <type>
                #:offset-type <type>
                <BRANCH> ...+ )

			

<BRANCH> ::= [<TRANSITION> <COND>]


<TRANSITION> ::= (<STATE> -> <STATE>)
             | (_ -> <STATE>)
             | (<id>)

<COND> ::=
        | <COUNTS> in <STATE>
        | <COND> and <COND>
        | <COND> or <COND>
        | not <COND>


<COUNTS> ::= (<natural> <natural> ...)
         | <natural>
         | none
         | some
         | all


<STATE> ::= <expr>

<CELL> ::= <expr>

<OFFSET> ::= <expr>
```
