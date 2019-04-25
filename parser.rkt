#lang plai

(print-only-errors)

;; Expr ::= <num>
;;       |  <bool>
;;       |  <id>
;;       |  (+ <Expr> <Expr>)
;;       |  (- <Expr> <Expr>)
;;       |  (* <Expr> <Expr>)
;;       |  (let [<id> <Expr>] <Expr>)
;;       |  (<id> <Expr>)
;;       |  (define (<id> <id>) <Expr>)

(define RESERVED-KEYWORDS '(+ - * define let))

(define-type Expr
  [num (? number?)]
  [bool (? boolean?)]
  [id (? valid-id?)]
  [add (lhs Expr?) (rhs Expr?)]
  [mult (lhs Expr?) (rhs Expr?)]
  [sub (lhs Expr?) (rhs Expr?)]
  [let-expr (id symbol?) (named-expr Expr?) (bound-body Expr?)]
  [call-expr (fun-name symbol?) (fun-arg Expr?)])

;; Type for a function definition, e.g.
;; (define (<fun-name> <arg-name) <body>)
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body Expr?)])

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

;; parse : String -> Expr
;; parses a sequence of strings to an Expr
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? boolean?) (bool sexp)]
    [(? valid-id?) (id sexp)]
    [(list '+ lhs rhs) (add (parse lhs) (parse rhs))]
    [(list '- lhs rhs) (sub (parse lhs) (parse rhs))]
    [(list '* lhs rhs) (mult (parse lhs) (parse rhs))]
    [(list 'let (list (? valid-id? id) binding-expr) body-expr)
     (let-expr id (parse binding-expr) (parse body-expr))]
    [(list (? symbol? fun-name) fun-arg) (call-expr fun-name (parse fun-arg))]
    [_ (error "Unable to parse string: " sexp)]))

(test (parse 1) (num 1))
(test (parse '(+ 1 2)) (add (num 1) (num 2)))
(test (parse '(- 1 2)) (sub (num 1) (num 2)))
(test (parse '(* (+ 1 2) 4)) (mult (add (num 1) (num 2)) (num 4)))
(test (parse #t) (bool #t))
(test (parse #f) (bool #f))
(test (parse '(let [foo 2] 5)) (let-expr 'foo (num 2) (num 5)))
(test (parse '(let [foo (let [foo 2] 5)] 5))
      (let-expr 'foo (let-expr 'foo (num 2) (num 5)) (num 5)))
(test (parse '(some-fun 4))
      (call-expr 'some-fun (num 4)))
(test (parse '(another-fun (+ 1 (- 1 2))))
      (call-expr 'another-fun (add (num 1) (sub (num 1) (num 2)))))

(test/exn (parse '+) "")

;; parse-fundef : sexp -> FunDef
;; parses a given sexp (symbolic expression) to the corresponding function definition
(define (parse-fundef sexp)
  (match sexp
    [(list 'define (list fun-name arg-name) body)
     (fundef fun-name arg-name (parse body))]))

(test (parse-fundef '(define (foo x) 1)) (fundef 'foo 'x (num 1)))
(test (parse-fundef '(define (bar x) (let [y 1] (+ x y))))
      (fundef 'bar 'x (let-expr 'y (num 1) (add (id 'x) (id 'y)))))

(define TEST-FUNDEFS (map parse-fundef '((define (foo x) (+ x x))
                                         (define (bar x) (let [y 1] (+ x y)))
                                         (define (baz y) (+ (bar 1) 2))
                                         (define (bar2 x) 99))))

;; lookup-fundef : symbol (listof FunDef) -> FunDef
;; looks for a fundef with the given name as a symbol in a list of function definitions (fun-table)

(define (lookup-fundef name fun-table)
  (cond [(empty? fun-table) (error "Undefined identifier: " name)]
        [else
         (local [(define entry (first fun-table))]
           (if (symbol=? (fundef-fun-name entry) name)
               entry
               (lookup-fundef name (rest fun-table))))]))

(test (lookup-fundef 'baz TEST-FUNDEFS) (fundef 'baz 'y (add (call-expr 'bar (num 1)) (num 2))))
(test (lookup-fundef 'foo TEST-FUNDEFS) (fundef 'foo 'x (add (id 'x) (id 'x))))
(test/exn (lookup-fundef 'some-fun TEST-FUNDEFS) "")
(test/exn (lookup-fundef 'baz empty) "")
