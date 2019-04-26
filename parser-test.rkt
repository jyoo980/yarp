#lang plai

(require "parser.rkt")

;; valid-id? : tests
(test (valid-id? '+) false)
(test (valid-id? '-) false)
(test (valid-id? '*) false)
(test (valid-id? 'define) false)
(test (valid-id? 'let) false)
(test (valid-id? 'foo) true)

;; parse : tests
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

;; parse-fundef : tests
(test (parse-fundef '(define (foo x) 1)) (fundef 'foo 'x (num 1)))
(test (parse-fundef '(define (bar x) (let [y 1] (+ x y))))
      (fundef 'bar 'x (let-expr 'y (num 1) (add (id 'x) (id 'y)))))

;; lookup-fundef : tests
(test (lookup-fundef 'baz TEST-FUNDEFS) (fundef 'baz 'y (add (call-expr 'bar (num 1)) (num 2))))
(test (lookup-fundef 'foo TEST-FUNDEFS) (fundef 'foo 'x (add (id 'x) (id 'x))))
(test/exn (lookup-fundef 'some-fun TEST-FUNDEFS) "")
(test/exn (lookup-fundef 'baz empty) "")
