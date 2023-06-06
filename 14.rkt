#lang pl 14

;;; ==================================================================
;;; Syntax

#| The BNF:
   <TOY> ::= <num>
           | <id>
           | { set! <id> <TOY> }
           | { bind {{ <id> <TOY> } ... } <TOY> <TOY> ... }
           | { bindrec {{ <id> <TOY> } ... } <TOY> <TOY> ... }
           | { fun { <id> ... } <TOY> <TOY> ... }
           | { rfun { <id> ... } <TOY> <TOY> ... }
           | { if <TOY> <TOY> <TOY> }
           | { <TOY> <TOY> ... }
|#

;; A matching abstract syntax tree datatype:
(define-type TOY
  [Num  Number]
  [Id   Symbol]
  [Set  Symbol TOY]
  [Bind    (Listof Symbol) (Listof TOY) (Listof TOY)]
  [BindRec (Listof Symbol) (Listof TOY) (Listof TOY)]
  [Fun  (Listof Symbol) (Listof TOY)]
  [RFun (Listof Symbol) (Listof TOY)]
  [Call TOY (Listof TOY)]
  [If   TOY TOY TOY])

(: unique-list? : (Listof Any) -> Boolean)
;; Tests whether a list is unique, guards Bind and Fun values.
(define (unique-list? xs)
  (or (null? xs)
      (and (not (member (first xs) (rest xs)))
           (unique-list? (rest xs)))))

(: parse-sexpr : Sexpr -> TOY)
;; parses s-expressions into TOYs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'set! more)
     (match sexpr
       [(list 'set! (symbol: name) new) (Set name (parse-sexpr new))]
       [else (error 'parse-sexpr "bad `set!' syntax in ~s" sexpr)])]
    [(cons (and binder (or 'bind 'bindrec)) more)
     (match sexpr
       [(list _ (list (list (symbol: names) (sexpr: nameds)) ...)
          body0 body ...)
        (if (unique-list? names)
          ((if (eq? 'bind binder) Bind BindRec)
           names
           (map parse-sexpr nameds)
           (map parse-sexpr (cons body0 body)))
          (error 'parse-sexpr "duplicate `~s' names: ~s" binder names))]
       [else (error 'parse-sexpr "bad `~s' syntax in ~s"
                    binder sexpr)])]
    [(cons (and funner (or 'fun 'rfun)) more)
     (match sexpr
       [(list _ (list (symbol: names) ...)
          body0 body ...)
        (if (unique-list? names)
          ((if (eq? 'fun funner) Fun RFun)
           names
           (map parse-sexpr (cons body0 body)))
          (error 'parse-sexpr "duplicate `~s' names: ~s" funner names))]
       [else (error 'parse-sexpr "bad `~s' syntax in ~s"
                    funner sexpr)])]
    [(cons 'if more)
     (match sexpr
       [(list 'if cond then else)
        (If (parse-sexpr cond) (parse-sexpr then) (parse-sexpr else))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [(list fun args ...) ; other lists are applications
     (Call (parse-sexpr fun)
           (map parse-sexpr args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> TOY)
;; Parses a string containing an TOY expression to a TOY AST.
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;;; ==================================================================
;;; Values and environments

;; an environemnt is a list of lists of values -- no names
(define-type ENV = (Listof (Listof (Boxof VAL))))

(define-type VAL
  [BogusV]
  [RktV  Any]
  [FunV  Natural (ENV -> VAL) ENV Boolean] ; `byref?' flag
  [PrimV ((Listof VAL) -> VAL)])

;; a single bogus value to use wherever needed
(define the-bogus-value (BogusV))

(: extend-rec : (Listof (ENV -> VAL)) ENV -> ENV)
;; extends an environment with a new recursive frame (given compiled
;; expressions).
(define (extend-rec compiled-exprs env)
  (let* ([boxes   (map (lambda (x) (box the-bogus-value))
                       compiled-exprs)]
         [new-env (cons boxes env)])
    (for-each (lambda ([box : (Boxof VAL)] [compiled : (ENV -> VAL)])
                (set-box! box (compiled new-env)))
              boxes compiled-exprs)
    new-env))

(: find-index : Symbol (Listof (Listof Symbol))
                -> (U (List Natural Natural) #f))
;; consumes a variable name and an environment description, and
;; returns the location of the variable in the environment as a pair
;; of the frame number (zero is the current frame, one is the
;; previous, etc) and the location within this frame; returns #f if
;; the variable is not found
(define (find-index name bindings)
  #| Double-list-in-one-loop version
  ;; helper for searching for the right frame
  (: loop : (Listof Symbol) (Listof (Listof Symbol)) Natural Natural
            -> (U (List Natural Natural) #f))
  ;; the searching loop -- receive the first frame as a separate
  ;; argument, to avoid re-consing it on each iteration
  (define (loop frame env i j)
    (cond
      [(null? frame) (and (not (null? env))
                          (loop (first env) (rest env) (add1 i) 0))]
      [(eq? name (first frame)) (list i j)]
      [else (loop (rest frame) env i (add1 j))]))
  ;; start the search only if we have something to search through
  (and (not (null? bindings))
       (loop (first bindings) (rest bindings) 0 0))
  |#
  ;; Easier version using `index-of'
  (: loop : (Listof (Listof Symbol)) Natural
            -> (U (List Natural Natural) #f))
  (define (loop env i)
    (and (pair? env)
         (let ([j (index-of (first env) name)])
           (if j (list i j) (loop (rest env) (add1 i))))))
  (loop bindings 0))
;; tests for this (see notes in the homework text):
(test (find-index 'a '((a b c) () (c d e))) => '(0 0))
(test (find-index 'e '((a b c) () (c d e))) => '(2 2))
(test (find-index 'c '((a b c) () (c d e))) => '(0 2))
(test (find-index 'x '((a b c) () (c d e))) => #f)

(: unwrap-rktv : VAL -> Any)
;; helper for `racket-func->prim-val': unwrap a RktV wrapper in
;; preparation to be sent to the primitive function
(define (unwrap-rktv x)
  (cases x
    [(RktV v) v]
    [else (error 'racket-func "bad input: ~s" x)]))

(: racket-func->prim-val : Function -> VAL)
;; converts a racket function to a primitive evaluator function which
;; is a PrimV holding a ((Listof VAL) -> VAL) function.  (the
;; resulting function will use the list function as is, and it is the
;; list function's responsibility to throw an error if it's given a
;; bad number of arguments or bad input types.)
(define (racket-func->prim-val racket-func)
  (define list-func (make-untyped-list-function racket-func))
  (PrimV (lambda (args) (RktV (list-func (map unwrap-rktv args))))))

;; The global environment has a few primitives:
(: global-environment : (Listof (List Symbol VAL)))
(define global-environment
  (list (list '+ (racket-func->prim-val +))
        (list '- (racket-func->prim-val -))
        (list '* (racket-func->prim-val *))
        (list '/ (racket-func->prim-val /))
        (list '< (racket-func->prim-val <))
        (list '> (racket-func->prim-val >))
        (list '= (racket-func->prim-val =))
        ;; values
        (list 'true  (RktV #t))
        (list 'false (RktV #f))))

(: global-lookup : Symbol -> VAL)
;; looks for a name in the global-environment.
(define (global-lookup name)
  (let ([cell (assq name global-environment)])
    (if cell
      (second cell)
      (error 'global-lookup "no binding for ~s" name))))

;;; ==================================================================
;;; Compilation

;; A type for compile-time descriptions of the lexical environment
(define-type BINDINGS = (Listof (Listof Symbol)))

(: compiler-enabled? : (Boxof Boolean))
;; a global flag that can disable the compiler
(define compiler-enabled? (box #f))

(: compile-body : (Listof TOY) BINDINGS -> (ENV -> VAL))
;; compiles a list of expressions to a single Racket function.
(define (compile-body exprs bindings)
  (unless (unbox compiler-enabled?)
    (error 'compile-body "compiler disabled"))
  ;; this is the third option mentioned in the homework -- compile the
  ;; list of expressions into a single racket function.  (Note: relies
  ;; on the fact that the body is never empty.)
  (let ([compiled-1st (compile (first exprs) bindings)]
        [rest         (rest exprs)])
    (if (null? rest)
      compiled-1st
      (let ([compiled-rest (compile-body rest bindings)])
        (lambda (env)
          (let ([ignored (compiled-1st env)])
            (compiled-rest env))))))
  ;; the same thing, but using `foldl' to do the loop
  ;; (foldl (lambda ([expr : TOY] [compiled-prev : (ENV -> VAL)])
  ;;          (let ([compiled (compile expr bindings)])
  ;;            (lambda ([env : ENV])
  ;;              (define ignored (compiled-prev env))
  ;;              (compiled env))))
  ;;        (lambda (env) the-bogus-value)
  ;;        exprs)
  )

(: compile-get-boxes : (Listof TOY) BINDINGS
                       -> (ENV -> (Listof (Boxof VAL))))
;; utility for applying rfun
(define (compile-get-boxes exprs bindings)
  (: compile-getter : TOY -> (ENV -> (Boxof VAL)))
  (define (compile-getter expr)
    (cases expr
      [(Id name)
       (match (find-index name bindings)
         [(list i j)
          (lambda ([env : ENV]) (list-ref (list-ref env i) j))]
         [#f
          (define ignored (global-lookup name))
          (lambda ([env : ENV])
            (error 'compile "cannot call an rfun with a global: ~s"
                   name))])]
      [else
       (lambda ([env : ENV])
         (error 'call "rfun application with a non-identifier ~s"
                expr))]))
  (unless (unbox compiler-enabled?)
    (error 'compile-get-boxes "compiler disabled"))
  (let ([getters (map compile-getter exprs)])
    (lambda (env)
      (map (lambda ([get-box : (ENV -> (Boxof VAL))]) (get-box env))
           getters))))

(: compile : TOY BINDINGS -> (ENV -> VAL))
;; compiles TOY expressions to Racket functions.
(define (compile expr bindings)
  ;; convenient helper for running compiled code
  (: boxed-caller : ENV -> ((ENV -> VAL) -> (Boxof VAL)))
  (define (boxed-caller env)
    (lambda (compiled) (box (compiled env))))
  (: compile* : TOY -> (ENV -> VAL))
  (define (compile* expr)
    (compile expr bindings))
  (unless (unbox compiler-enabled?)
    (error 'compile "compiler disabled"))
  (cases expr
    [(Num n)
     (define r (RktV n)) ; create at compile time
     (lambda ([env : ENV]) r)]
    [(Id name)
     ;; deconstruct values here to save doing it at runtime
     (match (find-index name bindings)
       [(list i j)
        (lambda ([env : ENV])
          (unbox (list-ref (list-ref env i) j)))]
       [#f
        (define global (global-lookup name))
        (lambda ([env : ENV]) global)])]
    [(Set name new)
     (define compiled-new (compile new bindings))
     (match (find-index name bindings)
       [(list i j) ; same as above
        (lambda ([env : ENV])
          (set-box! (list-ref (list-ref env i) j)
                    (compiled-new env))
          the-bogus-value)]
       [#f
        (define ignored (global-lookup name))
        (error 'compile "cannot mutate a global ~s" name)])]
    [(Bind names exprs bound-body)
     (define compiled-exprs (map compile* exprs))
     (define compiled-body  (compile-body bound-body
                                         (cons names bindings)))
     (lambda ([env : ENV])
       (compiled-body
        (cons (map (boxed-caller env) compiled-exprs) env)))]
    [(BindRec names exprs bound-body)
     (define new-bindings (cons names bindings)) ; used twice!
     (define compiled-exprs (map (lambda ([expr : TOY])
                                   (compile expr new-bindings))
                                 exprs))
     (define compiled-body (compile-body bound-body new-bindings))
     (lambda ([env : ENV])
       (compiled-body (extend-rec compiled-exprs env)))]
    [(Fun names bound-body)
     (define compiled-body (compile-body bound-body
                                         (cons names bindings)))
     (define arity (length names))
     (lambda ([env : ENV]) (FunV arity compiled-body env #f))]
    [(RFun names bound-body)
     (define compiled-body (compile-body bound-body
                                         (cons names bindings)))
     (define arity (length names))
     (lambda ([env : ENV]) (FunV arity compiled-body env #t))]
    [(Call fun-expr arg-exprs)
     (define compiled-fun  (compile fun-expr bindings))
     (define compiled-args (map compile* arg-exprs))
     (define compiled-boxes-getter (compile-get-boxes arg-exprs
                                                      bindings))
     (define arg-num (length arg-exprs))
     (lambda ([env : ENV])
       (define fval (compiled-fun env))
       (cases fval
         [(PrimV proc)
          (proc (map (lambda ([compiled-arg : (ENV -> VAL)])
                       (compiled-arg env))
                     compiled-args))]
         [(FunV arity compiled-body fun-env byref?)
          (if (= arity arg-num)
            (compiled-body
             (cons (if byref?
                     (compiled-boxes-getter env)
                     (map (boxed-caller env) compiled-args))
                   fun-env))
            (error 'call "arity mismatch in calling: ~s" fval))]
         [else (error 'call "function call with a non-function: ~s"
                      fval)]))]
    [(If cond-expr then-expr else-expr)
     (define compiled-cond (compile cond-expr bindings))
     (define compiled-then (compile then-expr bindings))
     (define compiled-else (compile else-expr bindings))
     (lambda ([env : ENV])
       ((if (cases (compiled-cond env)
              [(RktV v) v] ; Racket value => use as boolean
              [else #t])   ; other values are always true
          compiled-then
          compiled-else)
        env))]))

(: run : String -> Any)
;; compiles and runs a TOY program contained in a string
(define (run str)
  (set-box! compiler-enabled? #t)
  (let ([compiled (compile (parse str) '())])
    (set-box! compiler-enabled? #f)
    (let ([result (compiled '())])
      (cases result
        [(RktV v) v]
        [else (error 'run "the program returned a bad value: ~s"
                     result)]))))

;;; ==================================================================
;;; Tests

(test (run "{{fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}} {add3 1}}")
      => 4)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}
                   {add1 {fun {x} {+ x 1}}}}
              {bind {{x 3}} {add1 {add3 x}}}}")
      => 7)
(test (run "{bind {{identity {fun {x} x}}
                   {foo {fun {x} {+ x 1}}}}
              {{identity foo} 123}}")
      => 124)
(test (run "{bind {{x 3}}
              {bind {{f {fun {y} {+ x y}}}}
                {bind {{x 5}}
                  {f 4}}}}")
      => 7)
(test (run "{{{fun {x} {x 1}}
              {fun {x} {fun {y} {+ x y}}}}
             123}")
      => 124)

;; More tests for complete coverage
(test (run "{bind x 5 x}")      =error> "bad `bind' syntax")
(test (run "{fun x x}")         =error> "bad `fun' syntax")
(test (run "{if x}")            =error> "bad `if' syntax")
(test (run "{}")                =error> "bad syntax")
(test (run "{bind {{x 5} {x 5}} x}") =error> "duplicate*bind*names")
(test (run "{fun {x x} x}")     =error> "duplicate*fun*names")
(test (run "{+ x 1}")           =error> "no binding for")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{1 2}")             =error> "with a non-function")
(test (run "{{fun {x} x}}")     =error> "arity mismatch")
(test (run "{if {< 4 5} 6 7}")  => 6)
(test (run "{if {< 5 4} 6 7}")  => 7)
(test (run "{if + 6 7}")        => 6)
(test (run "{fun {x} x}")       =error> "returned a bad value")

;; assignment tests
(test (run "{set! {+ x 1} x}")  =error> "bad `set!' syntax")
(test (run "{bind {{x 1}} {set! x {+ x 1}} x}") => 2)

;; `bindrec' tests
(test (run "{bindrec {x 6} x}") =error> "bad `bindrec' syntax")
(test (run "{bindrec {{fact {fun {n}
                              {if {= 0 n}
                                1
                                {* n {fact {- n 1}}}}}}}
              {fact 5}}")
      => 120)

;; tests for multiple expressions and assignment
(test (run "{bind {{make-counter
                     {fun {}
                       {bind {{c 0}}
                         {fun {}
                           {set! c {+ 1 c}}
                           c}}}}}
              {bind {{c1 {make-counter}}
                     {c2 {make-counter}}}
                {* {c1} {c1} {c2} {c1}}}}")
      => 6)
(test (run "{bindrec {{foo {fun {}
                             {set! foo {fun {} 2}}
                             1}}}
              {+ {foo} {* 10 {foo}}}}")
      => 21)

;; `rfun' tests
(test (run "{{rfun {x} x} 4}") =error> "non-identifier")
(test (run "{bind {{swap! {rfun {x y}
                            {bind {{tmp x}}
                              {set! x y}
                              {set! y tmp}}}}
                   {a 1}
                   {b 2}}
              {swap! a b}
              {+ a {* 10 b}}}")
      => 12)

;; test that argument are not evaluated redundantly
(test (run "{{rfun {x} x} {/ 4 0}}") =error> "non-identifier")
(test (run "{5 {/ 6 0}}") =error> "non-function")

;; test compiler-disabled flag, for complete coverage
;; (these tests must use the functions instead of the toplevel `run',
;; since there is no way to get this error otherwise, this indicates
;; that this error should not occur outside of our code -- it is an
;; internal error check)
(test (compile (Num 1) '()) =error> "compiler disabled")
(test (compile-body (list (Num 1)) '()) =error> "compiler disabled")
(test (compile-get-boxes (list (Num 1)) '())
      =error> "compiler disabled")

;; test errors for inlined globals
(test (run "{set! + *}") =error> "cannot mutate a global")
;; test passing a global value to a user function, and error for rfun
(test (run "{{fun {f} {f 1 2}} +}") => 3)
(test (run "{{rfun {f} {f 1 2}} +}")
      =error> "cannot call an rfun with a global")
;; make sure that an unbound identifier is a compile-time error
(test (run "{{fun {x} 1} foo}")          =error> "no binding for")
(test (run "{{fun {x} 1} {fun {} foo}}") =error> "no binding for")

#; ; very cheap benchmark (remove the "#;" to run this)
(time (run "{bindrec {{fib {fun {n}
                             {if {< n 2}
                               n
                               {+ {fib {- n 1}}
                                  {fib {- n 2}}}}}}}
              {fib 27}}"))

;;; ==================================================================
