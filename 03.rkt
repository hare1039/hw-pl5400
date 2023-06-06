#lang pl 03

#| BNF for the PUWAE language:
     <PUWAE> ::= <num>
              | { + <PUWAE> <PUWAE> }
              | { - <PUWAE> <PUWAE> }
              | { * <PUWAE> <PUWAE> }
              | { / <PUWAE> <PUWAE> }
              | { with { <id> <PUWAE> } <PUWAE> }
              | <id>
              | { post <POST> ... }
     <POST> ::= <PUWAE> | + | - | * | /
|#

;; PUWAE abstract syntax trees
(define-type PUWAE
  [Num  Number]
  [Add  PUWAE PUWAE]
  [Sub  PUWAE PUWAE]
  [Mul  PUWAE PUWAE]
  [Div  PUWAE PUWAE]
  [Id   Symbol]
  [With Symbol PUWAE PUWAE]
  [Post (Listof PostfixItem)])

(define-type PostfixItem = (U PUWAE '+ '- '* '/))

(: parse-post-item : Sexpr -> PostfixItem)
;; parse an s-expression to a post-item
(define (parse-post-item x)
  (match x ['+ '+] ['- '-] ['* '*] ['/ '/] [else (parse-sexpr x)]))

(: parse-sexpr : Sexpr -> PUWAE)
;; parses s-expressions into PUWAEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'post more) (Post (map parse-post-item more))]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> PUWAE)
;; parses a string containing a PUWAE expression to a PUWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: subst : PUWAE Symbol PUWAE -> PUWAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (: post-subst : PostfixItem -> PostfixItem)
  (define (post-subst item)
    (if (symbol? item)
      item
      (subst item from to)))
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
             bound-body
             (subst bound-body from to)))]
    [(Post items) (Post (map post-subst items))]))

(: post-eval : (Listof PostfixItem) (Listof Number) -> Number)
;; evaluates a postfix sequence of items, using a stack
(define (post-eval items stack)
  (if (null? items)
    (match stack
      [(list result) result]
      [(list) (error 'post-eval "no value on the stack to return")]
      [else   (error 'post-eval "too many values left on the stack")])
    (let ([1st  (first items)]
          [more (rest items)])
      (: pop2-and-apply : (Number Number -> Number) -> Number)
      (define (pop2-and-apply op)
        (match stack
          [(list 2nd 1st stack ...)
           (post-eval more (cons (op 1st 2nd) stack))]
          [else (error 'post-eval "insufficient stack values")]))
      (cond [(eq? '+ 1st) (pop2-and-apply +)]
            [(eq? '- 1st) (pop2-and-apply -)]
            [(eq? '* 1st) (pop2-and-apply *)]
            [(eq? '/ 1st) (pop2-and-apply /)]
            [else (post-eval more (cons (eval 1st) stack))]))))

(: eval : PUWAE -> Number)
;; evaluates PUWAE expressions by reducing them to numbers
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (Num (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Post items) (post-eval items '())]))

(: run : String -> Number)
;; evaluate a PUWAE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x 5} {+ x x}}") => 10)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)
(test (run "{with {x 1} y}") =error> "free identifier")

;; additional tests for complete coverage
(test (run "{/ 4 2}") => 2)
(test (run "{with {x 2} {/ {* x x} x}}") => 2)
(test (run "{with x = 2 {+ x 3}}") =error> "bad `with' syntax")
(test (run "{bleh}") =error> "bad syntax")

;; more tests for the new post functionality, including expressions
;; given in the HW
(test (run "{post 2 3 +}") => 5)
(test (run "{post 1 2 + 3 4 + *}") => 21)
(test (run "{* {post 1 2 +} {post 3 4 +}}") => 21)
(test (run "{post 1 2 + {+ 3 4} *}") => 21)
(test (run "{post {post 1 2 +} {post 3 4 +} *}") => 21)
(test (run "{* {+ {post 1} {post 2}} {+ {post 3} {post 4}}}") => 21)
(test (run "{with {x {post 3 4 +}} {post 1 2 + x *}}") => 21)

;; tests for expressions that parse fine, but throw a runtime error
(test (run "{post 1 + *}")   =error> "insufficient stack values")
(test (run "{post 1 2 3}")   =error> "too many values left")
(test (run "{post 1 2 3 +}") =error> "too many values left")
(test (run "{post * * *}")   =error> "insufficient stack values")
(test (run "{post + 1 2}")   =error> "insufficient stack values")
(test (run "{post 1 + 9}")   =error> "insufficient stack values")
(test (run "{post}")         =error> "no value on the stack")

;; additional tests for non-commutative operators
(test (run "{post 3 1 -}") => 2)
(test (run "{post 6 3 /}") => 2)
