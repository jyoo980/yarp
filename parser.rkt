#lang plai

(print-only-errors)

;; Expr ::= <num>
;;       |  <bool>
;;       |  (+ <Expr> <Expr>)
;;       |  (- <Expr> <Expr>)
;;       |  (* <Expr> <Expr>)

(define-type Expr
  [num (? number?)]
  [bool (? boolean?)]
  [add (lhs Expr?) (rhs Expr?)]
  [mult (lhs Expr?) (rhs Expr?)])

;; parse-binop : Symbol Expr Expr -> Expr
;; parses a grammar of form (OP <Expr> <Expr>) into its correct representation
(define (parse-binop op lhs rhs)
  (cond [(symbol=? op '+) (add lhs rhs)]
        [(symbol=? op '*) (mult lhs rhs)]))

(test (parse-binop '+ (num 1) (num 2)) (add (num 1) (num 2)))
(test (parse-binop '* (num 1) (num 2)) (mult (num 1) (num 2)))

;; parse : String -> Expr
;; parses a sequence of strings to an Expr
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? boolean?) (bool sexp)]
    [`(,op ,lhs ,rhs) (parse-binop op (parse lhs) (parse rhs))]))

(test (parse 1) (num 1))
(test (parse '(+ 1 2)) (add (num 1) (num 2)))
(test (parse '(* (+ 1 2) 4)) (mult (add (num 1) (num 2)) (num 4)))
(test (parse #t) (bool #t))
(test (parse #f) (bool #f))
