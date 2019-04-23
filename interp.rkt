#lang plai

(require "parser.rkt")

;; interp : String -> <TODO>
;; interpreter for the Expr grammar
(define (interp expr)
  (type-case Expr expr
    [num (n) n]
    [bool (b) b]
    [id (name) name]
    [add (lhs rhs) (+ (interp lhs) (interp rhs))]
    [mult (lhs rhs) (* (interp lhs) (interp rhs))]
    [sub (lhs rhs) (- (interp lhs) (interp rhs))]))

(test (interp (num 1)) 1)
(test (interp (add (num 1) (num 2))) (+ 1 2))
(test (interp (mult (add (num 1) (num 2)) (num 44))) (* (+ 1 2) 44))
(test (interp (bool #t)) #t)
(test (interp (bool #f)) #f)
(test (interp (id 'fun)) 'fun)
(test (interp (sub (num 1) (num 3))) -2)

