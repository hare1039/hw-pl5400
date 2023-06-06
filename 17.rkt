#lang pl 17

;; Convenient type
(define-type Token = (U Symbol Integer))

;; -------------------------------------------------------------------
;; Fixed DFA version

;; A macro that defines a DFA language
(define-syntax automaton
  (syntax-rules (: ->)
    [(automaton init-state accepting-state
       [state : (input-sym -> new-state) ...]
       ...)
     (lambda (string)
       (: state : (Listof Token) -> Boolean)
       ...
       (define (state stream)
         (match stream
           ['() (eq? 'state 'accepting-state)]
           ;; Another way to do this is as follows:
           ;;   ['() (eq? state accepting-state)]
           ;; This would lead to the coverage problem since it uses
           ;; `state' as is -- a function value rather than a symbol.
           ;; Adding the test below resolves this problem since it leads
           ;; to a comparison with the initial state, but doing this is
           ;; broken since comparing function values is a bad idea as
           ;; described in the text.
           [(cons 'input-sym more) (new-state more)]
           ...
           [_ #f]))
       ...
       (init-state (explode-string string)))]))

(: cXr : String -> Boolean)
;; Identifies strings that match "c[ad]*r+"
(define cXr (automaton init end
              [init : (c -> more)]
              [more : (a -> more)
                      (d -> more)
                      (r -> end)]
              [end  : (r -> end)]))
;; tests:
(test (cXr "cadr"))
(test (cXr "cadadadadadadddddaaarrr"))
(test (not (cXr "ccadr")))
(test (not (cXr "c"))) ; passes now
(test (cXr "cr"))
(test (not (cXr ""))) ; might be needed for coverage, see above

(: div5 : String -> Boolean)
;; Determine whether a binary number is divisible by 5
(define div5
  (automaton mod0 mod0
    [mod0 : (0 -> mod0) (1 -> mod1)]
    [mod1 : (0 -> mod2) (1 -> mod3)]
    [mod2 : (0 -> mod4) (1 -> mod0)]
    [mod3 : (0 -> mod1) (1 -> mod2)]
    [mod4 : (0 -> mod3) (1 -> mod4)]))
(test (div5 ""))
(test (div5 "0"))
(test (div5 "000"))
(test (div5 (number->string 12345 2)))
(test (not (div5 (number->string 123453 2))))
(: RANDOM-TESTS : Integer -> Boolean)
(define (RANDOM-TESTS n)
  (define r (random 1000000000))
  (or (zero? n)
      (and (equal? (div5 (number->string r 2)) (zero? (modulo r 5)))
           (RANDOM-TESTS (sub1 n)))))
(test (RANDOM-TESTS 1000))

(: bogus : String -> Boolean)
(define bogus ; this machine never accepts!
  (lambda (string)
    (: init : (Listof Token) -> Boolean)
    (define (init stream)
      (match stream
        ['() (eq? 'init 'dummy)]
        [(cons 'x more) (init more)]
        [_ #f]))
    (init (explode-string string))))
(test (not (bogus "bleh")))
(test (not (bogus "xxx")))

;; -------------------------------------------------------------------
;; PDA implementation

;; A macro that defines a PDA language
(define-syntax pushdown
  (syntax-rules (: ->)
    [(pushdown init-state accepting-state
       [state : ((input-sym ...) (stack-sym ...)
                 -> new-state (new-stack-sym ...))
                ...]
       ...)
     (lambda (string)
       (: state : (Listof Token) (Listof Token) -> Boolean)
       ...
       (define (state stream stack)
         (match (list stream stack)
           [(list '() '()) (eq? 'state 'accepting-state)]
           [(list (list-rest 'input-sym ... more-input)
                  (list-rest 'stack-sym ... more-stack))
            (new-state more-input
                       (append '(new-stack-sym ...) more-stack))]
           ...
           [_ #f]))
       ...
       (init-state (append (explode-string string) '(*)) '(*)))]))

(: balanced : String -> Boolean)
;; Identifies strings that contain only balanced parentheses
(define balanced (pushdown init init
                   [init : ((open) ()      -> init (open))
                           ((close) (open) -> init ())
                           ((*) (*)        -> init ())]))
;; tests:
(test (balanced ""))
(test (balanced "()"))
(test (balanced "(((())))"))
(test (balanced "((()())(()))"))
(test (not (balanced "(")))
(test (not (balanced ")")))
(test (not (balanced ")(")))

(: zeros=ones : String -> Boolean)
;; Identifies strings of n 0s followed by n 1s
(define zeros=ones
  (pushdown 0s end
    [0s  : ((0) ()  -> 0s  (A))
           (()  ()  -> 1s  ())]
    [1s  : ((1) (A) -> 1s  ())
           ((*) (*) -> end (*))]
    [end : (()  (*) -> end ())]))
;; tests:
(test (zeros=ones ""))
(test (zeros=ones "01"))
(test (zeros=ones "000111"))
(test (not (zeros=ones "0")))
(test (not (zeros=ones "11")))
(test (not (zeros=ones "10")))
(test (not (zeros=ones "00011")))
(test (not (zeros=ones "00101111")))
