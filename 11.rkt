#lang pl 11

;; The simple Y definition (untyped and strict)
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (z) ((x x) z)))))))


;; -------------------------------------------------------------------
;; A `define/rec' rewrite rule that makes it possible to define
;; multiple-argument recursive functions.

(rewrite (define/rec (f x ...) E)
         => (define f
              (let ([g (Y (lambda (f)
                            (lambda (_)
                              (lambda (x ...)
                                (let ([f (f #f)])
                                  E)))))])
                (g #f))))

#|
;; This is an alternative version that uses the first argument as
;; usual, it is a little more complicated, and it doesn't work for
;; zero-argument functions.
(rewrite (define/rec (f x y ...) E)
         => (define f
              (let ([g (Y (lambda (f)
                            (lambda (x)
                              (lambda (y ...)
                                (let ([f (lambda (x y ...)
                                           ((f x) y ...))])
                                  E)))))])
                (lambda (x y ...) ((g x) y ...)))))
|#

;; trying the form, and testing that it works fine.

;; ackermann : Integer Integer -> Integer
;; computes the Ackermann function.
(define/rec (ackermann m n)
  (cond [(zero? m) (+ n 1)]
        [(zero? n) (ackermann (- m 1) 1)]
        [else      (ackermann (- m 1) (ackermann m (- n 1)))]))

(test (ackermann 3 3) => 61)

;; -------------------------------------------------------------------
;; A `letfuns' rule that allows binding several mutually recursive
;; functions, with multiple arguments.

#|
;; Simple version that works for single argument functions only:
(rewrite (letfuns ([(f x) E] ...) B)
         => (let ([g (Y (lambda (funs)
                          (lambda (name)
                            (match name
                              ['f (lambda (x)
                                    (let ([f (funs 'f)] ...)
                                      E))]
                              ...))))])
              (let ([f (g 'f)] ...)
                B)))
|#

;; A simple change makes it work with any arity functions:
(rewrite (letfuns ([(f x ...) E] ...) B)
         => (let ([g (Y (lambda (funs)
                          (lambda (name)
                            (match name
                              ['f (lambda (x ...)
                                    (let ([f (funs 'f)] ...)
                                      E))]
                              ...))))])
              (let ([f (g 'f)] ...)
                B)))

;; testing that it works with single arguments and more
(test (not (letfuns ([(even? n) (if (= n 0) #t (odd?  (- n 1)))]
                     [(odd?  n) (if (= n 0) #f (even? (- n 1)))])
             ;; adding a test to get complete coverage
             (or (even? 123) (not (even? 124))))))
(test (letfuns ([(even? n m) (if (= n 0) m  (odd?  (- n 1) (add1 m)))]
                [(odd?  n m) (if (= n 0) #f (even? (- n 1) m))])
        ;; here too
        (and (not (even? 11 0)) (even? 124 0)))
      => 62)

;; an extended example
(define scan
  (letfuns ([(start str)  (loop (explode-string str) 0)]
            [(loop l n)   (match l
                            [(list)
                             (zero? n)]
                            [(cons 'open more)
                             (loop more (add1 n))]
                            [(cons 'close more)
                             (and (> n 0) (loop more (sub1 n)))]
                            [(cons (number: m) more)
                             (nums more m n)]
                            [(cons _ more)
                             (loop more n)])]
            [(nums l m n) (match l
                            [(cons (number: m1) more)
                             (and (< m m1) (nums more m1 n))]
                            [else (loop l n)])])
    start))
(test (scan "(()123(246x12)) (blah)"))
(test (not (scan "(1232)")))
(test (not (scan "()(")))
(test (not (scan "())")))
