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
                            (subst body-expr target-id target-val)))]
    [call-expr (fun-name fun-args)
               (call-expr fun-name (subst target-id target-val fun-args))]))

(test (subst 'x (num 1) (add (num 1) (num 4))) (add (num 1) (num 4)))
(test (subst 'y (num 4) (add (id 'x) (id 'y))) (add (id 'x) (num 4)))
(test (subst 'x  (num 4) (add (id 'x) (num 4))) (add (num 4) (num 4)))

;; interp : String -> Number | Boolean
;; interpreter for the Expr grammar
(define (interp expr fundefs)
  (local [(define (inner-interp expr)
            (type-case Expr expr
              [num (n) n]
              [bool (b) b]
              [id (name) (error "Unbound identifier: " name)]
              [add (lhs rhs) (+ (inner-interp lhs) (inner-interp rhs))]
              [mult (lhs rhs) (* (inner-interp lhs) (inner-interp rhs))]
              [sub (lhs rhs) (- (inner-interp lhs) (inner-interp rhs))]
              [let-expr (name val body)
                        (inner-interp (subst name
                                       (num (inner-interp val))
                                       body))]
              [call-expr (fun-name arg-expr)
                         (interp-call fun-name arg-expr)]))
          
          (define (interp-call name arg-expr)
            (type-case FunDef (lookup-fundef name fundefs)
              [fundef (ident arg body)
                      (inner-interp (subst arg
                                           (num (inner-interp arg-expr))
                                           body))]))]
    (inner-interp expr)))

(test (interp (num 1) TEST-FUNDEFS) 1)
(test (interp (add (num 1) (num 2)) TEST-FUNDEFS) (+ 1 2))
(test (interp (mult (add (num 1) (num 2)) (num 44)) TEST-FUNDEFS) (* (+ 1 2) 44))
(test (interp (bool #t) TEST-FUNDEFS) #t)
(test (interp (bool #f) TEST-FUNDEFS) #f)
(test (interp (sub (num 1) (num 3)) TEST-FUNDEFS) -2)
(test (interp (let-expr 'foo (num 2) (num 5)) TEST-FUNDEFS) 5)
(test (interp (let-expr 'foo (num 5) (add (id 'foo) (num 11))) TEST-FUNDEFS) 16)
(test (interp (parse '(let [x (let [y 2] y)] (+ x 11))) TEST-FUNDEFS) 13)
(test (interp (parse '(bar2 3)) TEST-FUNDEFS) 99)
(test (interp (parse '(foo 5)) TEST-FUNDEFS) 10)
; (test (interp (parse '(bar 0)) TEST-FUNDEFS) 1) ; debug 
(test/exn (interp (id 'fun) TEST-FUNDEFS) "")
