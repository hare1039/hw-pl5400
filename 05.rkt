#lang pl 05

#| BNF for the ALGAE language:
     <PROGRAM> ::= { program <FUN> ... }
     <FUN>     ::= { fun <id> { <id> } <ALGAE> }
     <ALGAE> ::= <num>
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> <ALGAE> ... }
               | { / <ALGAE> <ALGAE> ... }
               | { with { <id> <ALGAE> } <ALGAE> }
               | <id>
               | True
               | False
               | { < <ALGAE> <ALGAE> }
               | { = <ALGAE> <ALGAE> }
               | { <= <ALGAE> <ALGAE> }
               | { if <ALGAE> <ALGAE> <ALGAE> }
               | { not <ALGAE> }
               | { and <ALGAE> ... }
               | { or <ALGAE> ... }
               | { call <id> <ALGAE> }
               | { quote <id> }
               | { vcall <ALGAE> <ALGAE> }
|#

;; ALGAE abstract syntax trees

(define-type PROGRAM
  [Funs (Listof FUN)])

(define-type FUN
  [Fun Symbol Symbol ALGAE])

(define-type ALGAE
  [Num    Number]
  [Bool   Boolean]
  [Add    (Listof ALGAE)]
  [Mul    (Listof ALGAE)]
  [Sub    ALGAE (Listof ALGAE)]
  [Div    ALGAE (Listof ALGAE)]
  [Id     Symbol]
  [With   Symbol ALGAE ALGAE]
  [Less   ALGAE ALGAE]
  [Equal  ALGAE ALGAE]
  [LessEq ALGAE ALGAE]
  [If     ALGAE ALGAE ALGAE]
  [Call   Symbol ALGAE]
  [Quote  Symbol]
  [VCall  ALGAE ALGAE])

(: parse-program : Sexpr -> PROGRAM)
;; parses a whole program s-expression into a PROGRAM
(define (parse-program sexpr)
  (match sexpr
    [(cons 'program funs) (Funs (map parse-fun funs))]
    [else (error 'parse-program "bad program syntax: ~s" sexpr)]))

(: parse-fun : Sexpr -> FUN)
;; parses a function s-expression syntax to an instance of FUN
(define (parse-fun sexpr)
  (match sexpr
    [(list 'fun (symbol: name) (list (symbol: arg)) body)
     (Fun name arg (parse-expr body))]
    [else (error 'parse-fun "bad function syntax: ~s" sexpr)]))

(: parse-expr : Sexpr -> ALGAE)
;; parses an s-expression into an ALGAE abstract syntax tree
(define (parse-expr sexpr)
  ;; utility for parsing a list of expressions
  (: parse-exprs : (Listof Sexpr) -> (Listof ALGAE))
  (define (parse-exprs sexprs) (map parse-expr sexprs))
  (match sexpr
    [(number: n)    (Num n)]
    ['True          (Bool #t)] ; \ check these before the next
    ['False         (Bool #f)] ; / case turns them into identifiers
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-expr named) (parse-expr body))]
       [else (error 'parse-expr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'call more)
     (match sexpr
       [(list 'call (symbol: name) arg) (Call name (parse-expr arg))]
       [else (error 'parse-expr "bad `call' syntax in ~s" sexpr)])]
    [(cons 'quote more)
     (match sexpr
       [(list 'quote (symbol: name)) (Quote name)]
       [else (error 'parse-expr "bad `quote' syntax in ~s" sexpr)])]
    [(list '+ args ...)     (Add (parse-exprs args))]
    [(list '* args ...)     (Mul (parse-exprs args))]
    [(list '- fst args ...) (Sub (parse-expr fst) (parse-exprs args))]
    [(list '/ fst args ...) (Div (parse-expr fst) (parse-exprs args))]
    [(list '<  lhs rhs)     (Less   (parse-expr lhs) (parse-expr rhs))]
    [(list '=  lhs rhs)     (Equal  (parse-expr lhs) (parse-expr rhs))]
    [(list '<= lhs rhs)     (LessEq (parse-expr lhs) (parse-expr rhs))]
    [(list 'if cond then else)
     (If (parse-expr cond) (parse-expr then) (parse-expr else))]
    [(list 'and args ...)  (And (parse-exprs args))]
    [(list 'or  args ...)  (Or  (parse-exprs args))]
    [(list 'not arg)       (Not (parse-expr arg))]
    [(list 'vcall lhs rhs) (VCall (parse-expr lhs) (parse-expr rhs))]
    [else (error 'parse-expr "bad syntax in ~s" sexpr)]))

(: Not : ALGAE -> ALGAE)
;; Translates `{not E}' syntax to core Algae.
(define (Not expr)
  (If expr (Bool #f) (Bool #t)))

(: And : (Listof ALGAE) -> ALGAE)
;; Translates `{and E ...}' syntax to core Algae.
(define (And exprs)
  (cond [(null? exprs) (Bool #t)]
        ;; Note: the following case is *not* redundant!
        [(null? (rest exprs)) (first exprs)]
        [else (If (car exprs) (And (cdr exprs)) (Bool #f))]))

(: Or : (Listof ALGAE) -> ALGAE)
;; Translates `{or E ...}' syntax to core Algae.
(define (Or exprs)
  (cond [(null? exprs) (Bool #f)]
        [(null? (cdr exprs)) (car exprs)] ; same here
        [else (If (car exprs) (Bool #t) (Or (cdr exprs)))]))

(: parse : String -> PROGRAM)
;; parses a string containing a ALGAE program to a PROGRAM instance
(define (parse str)
  (parse-program (string->sexpr str)))

(: subst : ALGAE Symbol ALGAE -> ALGAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  ;; convenient helper -- no need to specify `from' and `to'
  (: subst* : ALGAE -> ALGAE)
  (define (subst* x) (subst x from to))
  ;; helper to substitute lists
  (: substs* : (Listof ALGAE) -> (Listof ALGAE))
  (define (substs* exprs) (map subst* exprs))
  (cases expr
    [(Num n)        expr]
    [(Bool b)       expr]
    [(Quote name)   expr]
    [(Add args)     (Add (substs* args))]
    [(Mul args)     (Mul (substs* args))]
    [(Sub fst args) (Sub (subst* fst) (substs* args))]
    [(Div fst args) (Div (subst* fst) (substs* args))]
    [(Id name)      (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst* named-expr)
           (if (eq? bound-id from)
             bound-body
             (subst* bound-body)))]
    [(Less   lhs rhs) (Less   (subst* lhs) (subst* rhs))]
    [(Equal  lhs rhs) (Equal  (subst* lhs) (subst* rhs))]
    [(LessEq lhs rhs) (LessEq (subst* lhs) (subst* rhs))]
    [(If cond then else)
     (If (subst* cond) (subst* then) (subst* else))]
    [(Call fun arg)   (Call fun (subst* arg))]
    [(VCall fun arg)  (VCall (subst* fun) (subst* arg))]))

(define-type VAL = (U Number Boolean Symbol))

(: lookup-fun : Symbol PROGRAM -> FUN)
;; looks up a FUN instance in a PROGRAM given its name
(define (lookup-fun name prog)
  (cases prog
    [(Funs funs)
     (or (ormap (lambda ([fun : FUN])
                  ;; `ormap' will return the first true (= non-#f)
                  ;; result, so this is both a predicate and returning
                  ;; the value that is used
                  (cases fun
                    [(Fun fname arg expr)
                     (and (eq? fname name) fun)]))
                funs)
         (error 'lookup-fun "missing function definition for: ~s"
                name))]))

(: eval-number : ALGAE PROGRAM -> Number)
;; helper for `eval': verifies that the result is a number
(define (eval-number expr prog)
  (let ([result (eval expr prog)])
    (if (number? result)
      result
      (error 'eval-number
             "need a number when evaluating ~s, but got ~s"
             expr result))))

(: eval-boolean : ALGAE PROGRAM -> Boolean)
;; helper for `eval': verifies that the result is a boolean
(define (eval-boolean expr prog)
  (let ([result (eval expr prog)])
    (if (boolean? result)
      result
      (error 'eval-boolean
             "need a boolean when evaluating ~s, but got ~s"
             expr result))))

(: eval-symbol : ALGAE PROGRAM -> Symbol)
;; helper for `eval': verifies that the result is a symbol
(define (eval-symbol expr prog)
  (let ([result (eval expr prog)])
    (if (symbol? result)
      result
      (error 'eval-symbol
             "need a symbol when evaluating ~s, but got ~s"
             expr result))))

(: value->algae : VAL -> ALGAE)
;; converts a value to an ALGAE value (so it can be used with `subst')
(define (value->algae val)
  (cond [(number?  val) (Num val)]
        [(boolean? val) (Bool val)]
        [(symbol?  val) (Quote val)]))

(: eval : ALGAE PROGRAM -> VAL)
;; evaluates ALGAE expressions by reducing them to numbers
;; `prog' is provided for function lookup
(define (eval expr prog)
  (let ([eval         (lambda ([e : ALGAE]) (eval         e prog))]
        [eval-number  (lambda ([e : ALGAE]) (eval-number  e prog))]
        [eval-boolean (lambda ([e : ALGAE]) (eval-boolean e prog))]
        [eval-symbol  (lambda ([e : ALGAE]) (eval-symbol  e prog))])
    ;; convenient helper
    (: fold-evals : (Number Number -> Number) Number (Listof ALGAE)
                    -> Number)
    (define (fold-evals f init exprs)
      (foldl f init (map eval-number exprs)))
    (cases expr
      [(Num n) n]
      [(Bool b) b]
      [(Quote name) name]
      [(Add args) (fold-evals + 0 args)]
      [(Mul args) (fold-evals * 1 args)]
      [(Sub fst args)
       (let ([x (eval-number fst)])  ; need to evaluate in both cases
         (if (null? args) (- x) (- x (fold-evals + 0 args))))]
      [(Div fst args)
       (let ([x   (eval-number fst)] ; need to evaluate in both cases
             [div (fold-evals * 1 args)])
         (cond [(zero? (if (null? args) x div))
                (error '/ "division by zero error")]
               [(null? args) (/ x)]
               [else         (/ x div)]))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    ;; see the above `value->algae' helper
                    (value->algae (eval named-expr))))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Less   lhs rhs) (<  (eval-number lhs) (eval-number rhs))]
      [(Equal  lhs rhs) (=  (eval-number lhs) (eval-number rhs))]
      [(LessEq lhs rhs) (<= (eval-number lhs) (eval-number rhs))]
      [(If cond then else) (eval (if (eval-boolean cond) then else))]
      [(Call fun-name arg)
       (cases (lookup-fun fun-name prog)
         [(Fun name bound-id body)
          (eval (subst body bound-id (value->algae (eval arg))))])]
      [(VCall fun arg)
       (cases (lookup-fun (eval-symbol fun) prog)
         [(Fun name bound-id body)
          (eval (subst body bound-id (value->algae (eval arg))))])])))

(: run : String VAL -> VAL)
;; evaluate a complete ALGAE program contained in a string,
;; given a value to pass on to the `main' function
(define (run str arg)
  (let ([prog (parse str)])
    (eval (Call 'main (value->algae arg)) prog)))

(: run* : String -> VAL)
;; a version for testing simple ALGAE expressions without function
;; calls
(define (run* str)
  (eval (parse-expr (string->sexpr str)) (Funs null)))

;; tests (for simple expressions)
(test (run* "5") => 5)
(test (run* "{+ 5 5}") => 10)
(test (run* "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run* "{with {x 5} {+ x x}}") => 10)
(test (run* "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run* "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run* "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run* "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run* "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run* "{with {x 5} {with {y x} y}}") => 5)
(test (run* "{with {x 5} {with {x x} x}}") => 5)

;; additional tests for complete coverage (part 0)
(test (run* "x") =error> "free identifier")
(test (run* "{with {x 2} {/ 12 {* x 3}}}") => 2)
(test (run* "{with}") =error> "bad `with' syntax")
(test (run* "{foo}") =error> "bad syntax")
(test (run* "{}") =error> "bad syntax in")

;; test Racket-like arithmetics
(test (run* "{+}") => 0)
(test (run* "{*}") => 1)
(test (run* "{+ 10}") => 10)
(test (run* "{* 10}") => 10)
(test (run* "{- 10}") => -10)
(test (run* "{/ 10}") => 1/10)
(test (run* "{+ 1 2 3 4}") => 10)
(test (run* "{* 1 2 3 4}") => 24)
(test (run* "{- 10 1 2 3 4}") => 0)
(test (run* "{/ 24 1 2 3 4}") => 1)

;; test boolean comparators and `if'
(test (run* "{< 1 2}"))
(test (not (run* "{= 1 2}")))
(test (run* "{if {<= 4 4} 5 6}") => 5)
(test (run* "{if True False 6}") => #f)
(test (run* "{+ {< 1 2}}") =error> "need a number")
(test (run* "{if 1 2 3}") =error> "need a boolean")
(test (run* "{with {b {<= 4 5}} {if b b b}}") => #t)
(test (run* "{with {x 5} {if {< x 5} {= x 4} {<= x 7}}}"))
(test (run* "{with {b {= 3 4}} {with {x 5} {if b x x}}}") => 5)

;; test boolean extensions
;; (note how new tests use previously tested features)
(test (run* "{not {< 2 1}}"))
(test (not (run* "{not {not {< 2 1}}}")))
(test (run* "{and True True}"))
(test (run* "{not {and True False}}"))
(test (run* "{not {and False True}}"))
(test (run* "{not {and False False}}"))
(test (run* "{and {and {or True True}
                       {or True False}}
                  {and {or False True}
                       {not {or False False}}}}"))
(test (run* "{and 1 2 3}") =error> "need a boolean")
(test (not (run* "{and True {< 2 1} 3}")))
(test (run* "{and True {not {< 2 1}} 3}") => 3)
;; test proper short-circuiting
(test (run* "{or {/ 1 0} {< 1 2}}") =error> "division by zero")
(test (run* "{or {< 1 2} {/ 1 0}}"))
(test (run* "{not {and {/ 1 0} {< 2 1}}}") =error> "division by zero")
(test (run* "{not {and {< 2 1} {/ 1 0}}}"))
;; test arbitrary number of subexpressions for and/or
(test (run* "{and}"))
(test (run* "{and 1}") => 1)
(test (run* "{or 1}") => 1)
(test (run* "{not {or}}"))
(test (run* "{and True True True True}"))
(test (run* "{not {and True True False True}}"))
(test (run* "{or False False True False}"))
(test (run* "{not {and False False False False}}"))
(test (run* "{and {or True True}
                  {or True False}
                  {or False True}
                  {not {or False False}}}"))

;; test a real program
(test (run "{program
              {fun even? {n}
                {if {= 0 n} True {call odd? {- n 1}}}}
              {fun odd? {n}
                {if {= 0 n} False {call even? {- n 1}}}}
              {fun main {n}
                {if {= n 1}
                  1
                  {+ 1 {call main
                             {if {call even? n}
                               {/ n 2}
                               {+ 1 {* n 3}}}}}}}}"
           3)
      => 8)

;; test passing other types (including function names) to main
(test (run "{program {fun main {b} {if b 1 2}}}" #t) => 1)
(test (run "{program {fun main {b} {if b 1 2}}}" #f) => 2)
(test (run "{program {fun foo {n} {* n n}}
                     {fun main {f} {vcall f 3}}}" 'foo) => 9)

;; some test cases for full coverage
(test (run "1" 1)
      =error> "bad program syntax")
(test (run "{program 1}" 1)
      =error> "bad function syntax")
(test (run "{program {fun main {x} {call main}}}" 1)
      =error> "bad `call' syntax")
(test (run "{program {fun main {x} {call foo x}}}" 1)
      =error> "missing function definition")
(test (run "{program {fun main {x} {with {y 1} {+ x y}}}}" 1)
      => 2)
(test (run "{program {fun main {x} {with {foo 1} {call foo foo}}}
                     {fun foo {x} {- x -1}}}"
           1)
      => 2)
(test (run "{program
              {fun main {x}
                {*{+{*}{*}{*}{*}}{+{*}{*}{*}{*}}{+{*}{*}{*}{*}{*}}
                  {+{*}{*{+{*}{*}{*}}{+{*}{*}{*}}
                         {+{*}{*}{*}}{+{*}{*}}}}}}}"
           1)
      => 4400)

;; test that the language is not higher order
(test (run "{program {fun foo {foo} foo}
                     {fun main {foo} {call foo foo}}}"
           1)
      => 1)

;; test the vcall facility
(test (run "{program
              {fun even? {n}
                {if {= 0 n} True {call odd? {- n 1}}}}
              {fun odd? {n}
                {if {= 0 n} False {call even? {- n 1}}}}
              {fun do_even {n}
                {/ n 2}}
              {fun do_odd {n}
                {+ 1 {* n 3}}}
              {fun main {n}
                {if {= n 1}
                  1
                  {+ 1 {call main
                             {vcall {if {call even? n}
                                      {quote do_even}
                                      {quote do_odd}}
                                    n}}}}}}"
           3)
      => 8)
;; more tests for complete coverage
(test (run "{program {fun main {n} {quote 1}}}" 0)
      =error> "bad `quote' syntax")
(test (run "{program {fun foo {n} {+ n 1}}
                     {fun main {n}
                       {with {proc {quote foo}}
                         {vcall proc n}}}}"
           3)
      => 4)
(test (run "{program {fun main {n} {vcall 3 4}}}" 1)
      =error> "need a symbol")
