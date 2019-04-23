#lang plai

(print-only-errors)

;; Expr ::= <num>
;;       |  <bool>
;;       |  <id>
;;       |  (+ <Expr> <Expr>)
;;       |  (- <Expr> <Expr>)
;;       |  (* <Expr> <Expr>)

(define RESERVED-KEYWORDS '(+ - * define let))

(define-type Expr
  [num (? number?)]
  [bool (? boolean?)]
  [id (? valid-id?)]
  [add (lhs Expr?) (rhs Expr?)]
  [mult (lhs Expr?) (rhs Expr?)]
  [sub (lhs Expr?) (rhs Expr?)])

;; valid-id? : Symbol -> boolean
;; produce true iff the given symbol is a valid id
(define (valid-id? s)
  (and (symbol? s)
       (not (member s RESERVED-KEYWORDS))))

(test (valid-id? '+) false)
(test (valid-id? '-) false)
(test (valid-id? '*) false)
(test (valid-id? 'define) false)
(test (valid-id? 'let) false)
(test (valid-id? 'foo) true)

;; parse-binop : Symbol Expr Expr -> Expr
;; parses a grammar of form (OP <Expr> <Expr>) into its correct representation
(define (parse-binop op lhs rhs)
  (cond [(symbol=? op '+) (add lhs rhs)]
        [(symbol=? op '*) (mult lhs rhs)]
        [(symbol=? op '-) (sub lhs rhs)]))

(test (parse-binop '+ (num 1) (num 2)) (add (num 1) (num 2)))
(test (parse-binop '* (num 1) (num 2)) (mult (num 1) (num 2)))
(test (parse-binop '- (num 1) (num 2)) (sub (num 1) (num 2)))

;; parse : String -> Expr
;; parses a sequence of strings to an Expr
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? boolean?) (bool sexp)]
    [(? valid-id?) (id sexp)]
    [(list (? symbol? op) lhs rhs) (parse-binop op (parse lhs) (parse rhs))]
    [_ (error "Unable to parse string: " sexp)]))

(test (parse 1) (num 1))
(test (parse '(+ 1 2)) (add (num 1) (num 2)))
(test (parse '(- 1 2)) (sub (num 1) (num 2)))
(test (parse '(* (+ 1 2) 4)) (mult (add (num 1) (num 2)) (num 4)))
(test (parse #t) (bool #t))
(test (parse #f) (bool #f))
