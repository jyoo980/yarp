#lang plai

(require "parser.rkt")

;; subst : symbol Expr Expr -> Expr
;; substitution scheme for identifiers, replace all instances of the first arg (symbol)
;; which are present in the third arg (the body) with the second arg (value)
(define (subst target-id target-val bound-body)
  (type-case Expr bound-body
    [num (n) bound-body]
    [bool (b) bound-body]
    [id (name) (if (symbol=? name target-id)
                   target-val
                   bound-body)]
    [add (lhs rhs) (add (subst target-id target-val lhs)
                        (subst target-id target-val rhs))]
    [sub (lhs rhs) (sub (subst target-id target-val lhs)
                        (subst target-id target-val rhs))]
    [mult (lhs rhs) (mult (subst target-id target-val lhs)
                          (subst target-id target-val rhs))]
    [let-expr (bound-id named-expr body-expr)
              (if (symbol=? bound-id target-id)
                  (let-expr bound-id
                            (subst named-expr target-id target-val)
                            body-expr)
                  (let-expr bound-id
                            (subst named-expr target-id target-val)
                            (subst body-expr target-id target-val)))]))

(test (subst 'x (num 1) (add (num 1) (num 4))) (add (num 1) (num 4)))
(test (subst 'y (num 4) (add (id 'x) (id 'y))) (add (id 'x) (num 4)))
(test (subst 'x  (num 4) (add (id 'x) (num 4))) (add (num 4) (num 4)))

;; interp : String -> <TODO>
;; interpreter for the Expr grammar
(define (interp expr)
  (type-case Expr expr
    [num (n) n]
    [bool (b) b]
    [id (name) name]
    [add (lhs rhs) (+ (interp lhs) (interp rhs))]
    [mult (lhs rhs) (* (interp lhs) (interp rhs))]
    [sub (lhs rhs) (- (interp lhs) (interp rhs))]
    [let-expr (name val body) 'TODO]))

(test (interp (num 1)) 1)
(test (interp (add (num 1) (num 2))) (+ 1 2))
(test (interp (mult (add (num 1) (num 2)) (num 44))) (* (+ 1 2) 44))
(test (interp (bool #t)) #t)
(test (interp (bool #f)) #f)
(test (interp (id 'fun)) 'fun)
(test (interp (sub (num 1) (num 3))) -2)

