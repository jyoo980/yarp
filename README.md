# Yet Another Racket Parser

This is a very basic implementation of a parser/interpreter in Racket for Racket. Multiple components will be changed as more additions to the basic grammar are made. 

## Grammar

Below is the grammar that has been implemented in the parser so far

```
<Expr> ::= <num>
		|  <bool>
		|  <id>
		| '(' '+' <Expr> <Expr> ')'
		| '(' '-' <Expr> <Expr> ')'
		| '(' '*' <Expr> <Expr> ')'
		| '(' 'let' '[' <id> <Expr> ']' <Expr> ')' 
```
