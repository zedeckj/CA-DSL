#lang typed/racket
(require (for-syntax racket/syntax syntax/parse))
(require "types.rkt" "library/rule.rkt" "library/topologies.rkt" "library/neighborhoods.rkt")
(module+ test
(require syntax/macro-testing)
(require typed/rackunit))

(define-syntax (parse-condition stx)
  (syntax-parse stx
    [(_ neighbors:expr (count:expr ...+) (~datum in) state:expr) #'((inst has-neighbors-in-state? AliveOrDead) state neighbors (list count ...))]
    [(_ neighbors:expr count:expr (~datum in) state:expr) #'((inst has-neighbors-in-state? AliveOrDead) state neighbors (list count))]))

(define-syntax (parse-clauses stx)
  (syntax-parse stx
    [(_ neighbors:id state-type:id state:id 
      ((in-state:expr (~datum ->) out-state:expr) condition:expr ...+) clauses ...+)
      #'(let ([state-name : state-type out-state]) ; for type checking, even if shortcircuiting
      (if 
        (and (parse-condition neighbors condition ...) (equal? in-state state))
        state-name 
        (parse-clauses neighbors state-type state clauses ...)))]
      
    [(_ _ state-type:id _ ((~datum default) out-state:expr)) 
      #'(let ([state-name : state-type out-state]) state-name)]))


(define-syntax (rule stx)
  (syntax-parse stx
    [(_
      #:state-type state-type:id
      #:cell-type cell-type:id
      #:offset-type offset-type:id
      #:neighborhood neighborhood:expr
      clauses:expr ...)
        #'(lambda
                ([state-map : (StateMap cell-type state-type)]
                 [topology : (Topology cell-type offset-type)])
            (hash-map/copy state-map 
              (lambda ([cell : cell-type]
                       [in-state : state-type])
                (values cell
                  (let ([neighbors : (Listof state-type) (get-neighbors cell state-map topology neighborhood)])
                    (parse-clauses neighbors state-type in-state clauses ...))))))]))

(define-syntax (lifelike stx)
  (syntax-parse stx
    [(_ clauses:expr ...) 
    #'(rule 
    #:state-type AliveOrDead
    #:cell-type Posn
    #:offset-type Posn 
    #:neighborhood (moore-neighborhood)
    clauses ... [default 'dead])]))


#;(define (conway-rule state-map topology)
    (mapper state-map
        (lambda ([cell : Posn]
                [in-state : AliveOrDead])
            (let ([neighbors (get-neighbors cell state-map topology (moore-neighborhood))])
                (match in-state
                    ['alive (if (has-neighbors-in-state? 'alive neighbors '(2 3)) 'alive 'dead)]
                    ['dead (if (has-neighbors-in-state? 'alive neighbors '(3))'alive 'dead)])))))


#;(module+ test
  #;(assert-typecheck-fail
      (rule
    #:state-type Integer
    [(0 -> 'a) #f]
    [(1 -> 1) #t]
    [default 0]))
    
  (assert-typecheck-fail (let ([x : Integer 'a]) x)))
  
(define conways : (Rule Posn Posn AliveOrDead)
  (lifelike
    [('dead -> 'alive) 3 in 'alive]
    [('alive -> 'alive) (2 3) in 'alive]))

#;(define conways : (Rule Posn Posn AliveOrDead)
  (rule 
    #:state-type AliveOrDead
    #:cell-type Posn
    #:offset-type Posn
    #:neighborhood (moore-neighborhood)
    [('dead -> 'alive) 3 in 'alive]
    [('alive -> 'alive) (2 3) in 'alive]
    [default 'dead]))

(provide conways rule)
