#lang pl 07

#|

Tail call optimization is crucial for this implementation, because it
is the essential feature that makes tail function calls equivalent to
gotos.  In other words, the way we define labels and `goto' depends on
having tail call optimization to make our `goto' into an actual jump.

A simple way to demonstrate this is a plain loop:

    (: loop : -> Void)
    (define (loop)
      (: loop : Label)
      (define (loop) (goto loop))
      (loop))

If tail calls were not optimized, then this would fail to run forever,
since it would run out of stack space.  This means that it's not really
a simulation of an assembly language.  (Note, BTW, how this function
typechecks fine as returning a Void when in practice it never actually
returns...)

This implies that each and every of the "code blocks" (as implemented by
label functions) must have *exactly one* `goto` (or derivatives like one
of the `if*`s or `halt`), it it must be at the end of each block.  Not
including any `goto` is a little bad since it is an implied `halt`; but
including more than one is much worse: it basically (ab)uses them as a
kind of a function call (which is what they are since we're in Racket)
and therefore the result is no longer a kind of an assembly language.

|#


;; we represent labels (goto targets) as void thunks, and registers (or
;; memory locations in general) as integer boxes.
(define-type Label    = (-> Void))
(define-type Register = (Boxof Integer))

;; "X = Y"
;; assigns the contents of register Y to register X
(: mov : Register Register -> Void)
(define (mov X Y) (set-box! X (unbox Y)))

;; "X = N"
;; assigns the constant N (an "immediate" value)  to register X
(: movi : Register Integer -> Void)
(define (movi X N) (set-box! X N))

;; "X += Y"
;; increments register X by register Y
(: add : Register Register -> Void)
(define (add X Y) (set-box! X (+ (unbox X) (unbox Y))))

;; "X += N"
;; increments register X by a constant N
(: addi : Register Integer -> Void)
(define (addi X N) (set-box! X (+ (unbox X) N)))

;; "X -= Y"
;; decrements register X by register Y
(: sub : Register Register -> Void)
(define (sub X Y) (set-box! X (- (unbox X) (unbox Y))))

;; "X -= N"
;; decrements register X by a constant N
(: subi : Register Integer -> Void)
(define (subi X N) (set-box! X (- (unbox X) N)))

;; "X &= Y"
;; sets X to the bitwise "and" of X and Y
;; (: and : Register Register -> Void)
;; (define (and X Y) (set-box! X (bitwise-and (unbox X) (unbox Y))))

;; "X &= N"
;; sets X to the bitwise "and" of X and a constant N
(: andi : Register Integer -> Void)
(define (andi X N) (set-box! X (bitwise-and (unbox X) N)))

;; "X >>= N"
;; shifts register X right by N bits
(: shri : Register Integer -> Void)
(define (shri X N) (set-box! X (arithmetic-shift (unbox X) (- N))))

;; "goto L"
;; (goto L) jumps to the label -- labels are represented as nullary
;; functions (also called "thunks")
(: goto : Label -> Void)
(define (goto L) (L))

;; "halt"
;; halt execution, same as `void` (which is the trivial Racket function
;; that returns a Void value)
(: halt : -> Void)
(define halt void)

;; "if X=0 goto L1 else goto L2"
;; if register X is zero, jump to L1, else jump to L2
(: if0 : Register Label Label -> Void)
(define (if0 a l1 l2) (if (zero? (unbox a)) (goto l1) (goto l2)))

;; "if X>0 goto L1 else goto L2"
;; if register X is positive, jump to L1, else jump to L2
(: ifp : Register Label Label -> Void)
(define (ifp a l1 l2) (if (positive? (unbox a)) (goto l1) (goto l2)))

(: fib : Integer -> Integer)
;; compute the nth fibonacci number using the assembly language
(define (fib n)
  (: A : Register) (define A (box 0))
  (: B : Register) (define B (box 1))
  (: C : Register) (define C (box 0))
  (: N : Register) (define N (box n))
  ;;
  (: main : Label)
  (: step : Label)
  ;;
  (define (main) (if0  N halt step))
  (define (step) (mov  C A)
                 (add  C B)
                 (mov  A B)
                 (mov  B C)
                 (subi N 1)
                 (goto main))
  ;;
  (main)
  (unbox A))

;; test
(test (map fib '(0 1 2 3 4 5 6 7 8 9 10))
      => '(0 1 1 2 3 5 8 13 21 34 55))

(: more-ones? : Integer Integer -> Integer)
;; returns 1 if `a' has more 1s in its binary representation than `b'
(define (more-ones? a b)
  (: A : Register) (define A (box a))
  (: B : Register) (define B (box b))
  (: C : Register) (define C (box 0))
  (: D : Register) (define D (box 0))
  (: S : Register) (define S (box 0))
  (: T : Register) (define T (box 0))
  (: R : Register) (define R (box 0))
  ;;
  (: main   : Label)
  (: doneA  : Label)
  (: doneB  : Label)
  (: count  : Label)
  (: cloop  : Label)
  (: addbit : Label)
  (: cdone  : Label)
  (: done0  : Label)
  (: done1  : Label)
  ;;
  (define (main)   ;; sum A's bits into A
                   (mov  C A)
                   (movi S 0) ; use S for state (0,1)
                   (goto count))
  (define (doneA)  (mov  A D)
                   ;; sum B's into B
                   (mov  C B)
                   (movi S 1)
                   (goto count))
  (define (doneB)  (mov  B D)
                   ;; done, we have A and B as the counts
                   (sub  A B)
                   (ifp  A done1 done0))
  (define (done0)  (movi R 0)
                   (halt))
  (define (done1)  (movi R 1)
                   (halt))
  ;; counts 1s in C, stores the result in D
  (define (count)  (movi D 0)
                   (goto cloop))
  (define (cloop)  (if0  C cdone addbit))
  (define (addbit) (mov  T C)
                   (shri C 1)
                   (andi T 1)
                   (add  D T)
                   (goto cloop))
  ;; S=0 sum B now, else, done sums
  (define (cdone)  (if0  S doneA doneB))
  ;;
  (main)
  (unbox R))

;; tests
(test (more-ones? 0 0) => 0)
(test (more-ones? 1 0) => 1)
(test (more-ones? 1 2) => 0)
(test (more-ones? 2 0) => 1)
(test (more-ones? 0 1) => 0)
(test (more-ones? 0 2) => 0)
(test (more-ones? 2 1) => 0)
(test (more-ones? 2 2) => 0)
(test (more-ones? 3 1) => 1)
