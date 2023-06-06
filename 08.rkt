#lang pl 08

#|
The grammar:
  <BRANG> ::= <num>
            | { + <BRANG> <BRANG> }
            | { - <BRANG> <BRANG> }
            | { * <BRANG> <BRANG> }
            | { / <BRANG> <BRANG> }
            | { with { <id> <BRANG> } <BRANG> }
            | { bind {{ <id> <BRANG> } { <id> <BRANG> } ...} <BRANG> }
            | { bind* {{ <id> <BRANG> } { <id> <BRANG> } ...} <BRANG> }
            | <id>
            | { fun { <id> ... } <BRANG> }
            | { call <BRANG> <BRANG> ... }

Core evaluation rules:
  eval(N,env)                = N
  eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
  eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
  eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
  eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
  eval(CRef(N),env)          = list-ref(env,N)
  eval({fun {x} E},env)      = <{fun {x} E}, env>
  eval({call E1 E2},env1)    = eval(Ef,cons(eval(E2,env1),env2))
                               if eval(E1,env1) = <{fun {x} Ef}, env2>
                             = error!  otherwise
Note that these rules are incomplete, since they don't represent the
language that users actually see.
|#

(define-type BRANG
  [Num   Number]
  [Add   BRANG BRANG]
  [Sub   BRANG BRANG]
  [Mul   BRANG BRANG]
  [Div   BRANG BRANG]
  [Id    Symbol]
  [With  Symbol BRANG BRANG]
  [Bind  (Listof Symbol) (Listof BRANG) BRANG]
  [Bind* (Listof Symbol) (Listof BRANG) BRANG]
  [Fun   (Listof Symbol) BRANG]
  [Call  BRANG (Listof BRANG)])

(define-type CORE
  [CNum  Number]
  [CAdd  CORE CORE]
  [CSub  CORE CORE]
  [CMul  CORE CORE]
  [CDiv  CORE CORE]
  [CRef  Natural]
  [CFun  CORE]
  [CCall CORE CORE])

(: parse-sexpr : Sexpr -> BRANG)
;; parses s-expressions into BRANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons (or 'bind 'bind*) more)
     (match sexpr
       [(list binder (list (list (symbol: name)  named)
                           (list (symbol: names) (sexpr: nameds)) ...)
              body)
        ((if (eq? binder 'bind) Bind Bind*)
         (cons name names)
         (map parse-sexpr (cons named nameds))
         (parse-sexpr body))]
       [(cons binder more)
        (error 'parse-sexpr "bad `~s' syntax in ~s" binder sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: names) ...) body)
        (Fun names (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun args ...)
     (Call (parse-sexpr fun) (map parse-sexpr args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> BRANG)
;; parses a string containing a BRANG expression to a BRANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; These are the values of our language
(define-type VAL
  [NumV Number]
  [FunV CORE ENV])

;; An environment is a simple list of values
(define-type ENV = (Listof VAL))

;; Syntactic environments for the de-Bruijn preprocessing:
;; define a type and an empty environment

(define-type DE-ENV = (U Symbol #f) -> Natural)

(: de-empty-env : DE-ENV)
;; the empty syntactic environment, always throws an error
(define (de-empty-env id)
  (error 'de-env "Free identifier: ~s" id))

(: de-extend : DE-ENV (U Symbol #f) -> DE-ENV)
;; extends a given de-env for a new identifier
(define (de-extend env id)
  (lambda (name)
    (if (eq? id name)
      0
      (+ 1 (env name)))))
;; test, demonstrating how it should work
(test (let ([e (de-extend (de-extend de-empty-env 'b) 'a)])
        (map (lambda ([id : Symbol]) (e id))
             '(a b)))
      => '(0 1))

(: preprocess : BRANG DE-ENV -> CORE)
;; replaces identifier expressions into Ref AST values
(define (preprocess expr de-env)
  (: sub : BRANG -> CORE)
  (define (sub expr) (preprocess expr de-env))
  (cases expr
    [(Num n)   (CNum n)]
    [(Add l r) (CAdd (sub l) (sub r))]
    [(Sub l r) (CSub (sub l) (sub r))]
    [(Mul l r) (CMul (sub l) (sub r))]
    [(Div l r) (CDiv (sub l) (sub r))]
    [(With bound-id named-expr bound-body)
     (sub (Call (Fun (list bound-id) bound-body) (list named-expr)))]
    [(Bind names named-expressions body)
     (sub (Call (Fun names body) named-expressions))]
    [(Bind* names named-expressions body)
     ;; always strip off one binding into a `With'
     (sub (With (first names)
                (first named-expressions)
                (if (= 1 (length names))
                  body
                  (Bind* (rest names)
                         (rest named-expressions)
                         body))))]
    [(Id name) (CRef (de-env name))]
    [(Fun bound-ids bound-body)
     (let ([len (length bound-ids)])
       (cond
         ;; nullary functions: add a dummy, use #f so there's never a
         ;; name captured
         [(= len 0)
          (CFun (preprocess bound-body (de-extend de-env #f)))]
         ;; basic one-argument case
         [(= len 1)
          (CFun (preprocess bound-body
                            (de-extend de-env (car bound-ids))))]
         ;; multiple arguments
         [else (sub (Fun (list (car bound-ids))
                         (Fun (cdr bound-ids) bound-body)))]))]
    [(Call fun-expr arg-exprs)
     (let ([len (length arg-exprs)])
       (cond
         ;; nullary application: use a dummy value (0 in this case)
         [(= len 0) (sub (Call fun-expr (list (Num 0))))]
         ;; basic one-argument case
         [(= len 1) (CCall (sub fun-expr) (sub (car arg-exprs)))]
         ;; multiple arguments
         [else (sub (Call (Call fun-expr (list (car arg-exprs)))
                          (cdr arg-exprs)))]))]))

(: NumV->number : VAL -> Number)
;; convert a FLANG runtime numeric value to a Racket one
(define (NumV->number val)
  (cases val
    [(NumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" val)]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : CORE ENV -> VAL)
;; evaluates CORE expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(CNum n) (NumV n)]
    [(CAdd l r) (arith-op + (eval l env) (eval r env))]
    [(CSub l r) (arith-op - (eval l env) (eval r env))]
    [(CMul l r) (arith-op * (eval l env) (eval r env))]
    [(CDiv l r) (arith-op / (eval l env) (eval r env))]
    [(CRef n) (list-ref env n)]
    [(CFun bound-body) (FunV bound-body env)]
    [(CCall fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-body f-env)
          (eval bound-body (cons (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]))

(: run : String -> Number)
;; evaluate a BRANG program contained in a string
(define (run str)
  (let ([result (eval (preprocess (parse str) de-empty-env) null)])
    (cases result
      [(NumV n) n]
      [else (error 'run "evaluation returned a non-number: ~s"
                   result)])))

;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
      => 124)

;; test remaining arithmetic functions
(test (run "{call {fun {x} {- x 1}} 4}")
      => 3)
(test (run "{call {fun {x} {* x 3}} 4}")
      => 12)
(test (run "{call {fun {x} {/ x 2}} 4}")
      => 2)

;; test errors
(test (run "{call {fun {x} {? x 1}} 4}")
      =error> "bad syntax in")
(test (run "{call {fun {x} {+ y 1}} 4}")
      =error> "Free identifier: y")
(test (run "{call {fun {x} } 4}")
      =error> "bad `fun' syntax")
(test (run "{call {fun {x} } 4}")
      =error> "bad `fun' syntax")

(test (run "{with {y} }")
      =error> "bad `with' syntax")
(test (run "{fun {x} {+ x x}}")
      =error> "evaluation returned a non-number")
(test (run "{+}")
      =error> "bad syntax in (+)")
(test (run "{+ {fun {x} x} 1}")
      =error> "arith-op: expected a number")
(test (run "{call 1 1}")
      =error> "expects a function")

;; test multiple-argument functions
(test (run "{with {add {fun {x y} {+ x y}}} {call add 7 8}}")
      => 15)

;; tests that demonstrate three ways in which our multi-argument
;; extension is not like real multi-argument functions and
;; applications:
;; {fun {x x} ...} makes sense in our world
(test (run "{with {foo {fun {x x} {+ x 5}}}
              {call foo 9 8}}")
      => 13)
;; {fun {x} {fun {y} ...}} can be called with a single {call foo 2 3}
(test (run "{with {add {fun {x} {fun {y} {+ x y}}}}
              {call add 3 4}}")
      => 7)
;; {fun {x y} ...} can be called with one argument
(test (run "{with {add {fun {x y} {+ x y}}}
              {with {add3 {call add 3}}
                {call add3 4}}}")
      => 7)

;; test new binders
(test (run "{bind {x 1} 2}")
      =error> "bad `bind' syntax")
(test (run "{bind {} 2}")
      =error> "bad `bind' syntax")
(test (run "{bind* {x 1} 2}")
      =error> "bad `bind*' syntax")
(test (run "{bind* {} 2}")
      =error> "bad `bind*' syntax")
(test (run "{bind {{x 1} {y 2}} {+ x y}}")
      => 3)
(test (run "{bind {{x 1} {y 2}} {- y x}}")
      => 1)
(test (run "{bind* {{x 1} {x {+ x 1}} {x {* x 2}}} x}")
      => 4)
;; a test that demonstrates that bind is not bind*
(test (run "{bind {{x 1} {y 2}}
              {bind {{x y} {y x}}
                {- x y}}}")
      => 1)
(test (run "{bind* {{x 1} {y 2}}
              {bind* {{x y} {y x}}
                {- x y}}}")
      => 0)

;; Differences from a language with multiple arguments (first part),
;; demonstrated with tests that work for us, and would fail for a
;; language with different function arities:
;; * one difference is obvious: in our language we can partially call
;;   any function, and get back a function waiting for more inputs;
;;   this is not like in racket, for example, where we get an arity
;;   error:
(test (run "{call {call {fun {x y} 1} 2} 3}") => 1)
;; * another difference is that we can apply a function on more
;;   arguments than it was defined with -- we cannoth distinguish a
;;   function of four arguments from a function of two arguments that
;;   happened to return a function of two arguments:
(test (run "{call {fun {x} {fun {y} {+ x y}}} 1 2}") => 3)
;; * a third difference is that in our language we can use the same
;;   name multiple times for input: {fun {x x} x}, and have well
;;   specified semantics, whereas if we had multiple arguments
;;   implemented in the core this would depend on how we substitute,
;;   which is why programming languages usually choose to throw an
;;   error:
(test (run "{call {fun {x x} x} 1 2}") => 2)

;; three required tests that demonstrate the three problems with nullary
;; functions:
;; in this test, we verify that we do have a problem since we can call a
;; unary function with no arguments at all -- it will receive the dummy
;; value, so the essence of this test is in verifying that we don't
;; crash with an error; specifically, we don't test what this evaluates
;; to, since this depends on the actual dummy value that we've chosen.
;; Luckily, there are no boolean results here, so the test cannot get
;; confused by a #f result.
(test (run "{call {fun {x} x}}"))
;; another way to do this test and avoid depending on the dummy value is
;; to simply return some known value, since the only point of this test
;; is to show that no errors are thrown:
(test (run "{call {fun {x} 123}}") => 123)

;; if we call a nullary function with 1 argument, the argument will just
;; be ignored.
(test (run "{call {fun {} 123} 42}") => 123)

;; if we always plug in a `dummy' name, then this test will fail (well,
;; unless the dummy value happens to be 3).
(test (run "{call {with {dummy 3} {fun {} {* dummy dummy}}}}") => 9)
