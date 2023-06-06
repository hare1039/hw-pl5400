#lang pl 02

;;; ------------------------------------------------------------------
;;; Question 1

(: sequence : (All (A) (A -> A) A A -> (Listof A)))
;; Returns a list of values starting at `first' and ending at `last',
;; where each value is the result of applying `f' on the last value.
(define (sequence f first last)
  (if (equal? first last)
    (list first)
    (cons first (sequence f (f first) last))))
;; Alternatively, note that we're always consing `first', so this
;; definition works too:
;; (define (sequence f first last)
;;   (cons first (if (equal? first last)
;;                 '()
;;                 (sequence f (f first) last))))

;; tests
(test (sequence add1 1 1) => (list 1))
(test (sequence add1 1 5) => (list 1 2 3 4 5))
(test (sequence sub1 5 1) => (list 5 4 3 2 1))
(test (sequence sqrt 65536 2) => (list 65536 256 16 4 2))
(test (sequence not #f #t) => (list #f #t))
(test (sequence (inst rest Number) (list 1 2 3) null)
      => (list (list 1 2 3) (list 2 3) (list 3) null))

;;; ------------------------------------------------------------------
;;; Question 2

;; A type for integer sets
(define-type INTSET
  [Num   Integer]
  [Range Integer Integer]
  [2Sets INTSET INTSET])

(: intset-min/max : INTSET (Integer Integer -> Boolean) -> Integer)
;; Finds the minimal or maximal member of the given set, depending on
;; the given comparator.
(define (intset-min/max set compare)
  (cases set
    [(Num n)         n]
    [(Range lo hi)   (if (compare lo hi) lo hi)]
    [(2Sets 1st 2nd) (let ([1st (intset-min/max 1st compare)]
                           [2nd (intset-min/max 2nd compare)])
                       (if (compare 1st 2nd) 1st 2nd))]))

(: intset-min : INTSET -> Integer)
;; Finds the minimal member of the given set.
(define (intset-min set) (intset-min/max set <))

(: intset-max : INTSET -> Integer)
;; Finds the maximal member of the given set.
(define (intset-max set) (intset-min/max set >))

(: intset-normalized? : INTSET -> Boolean)
;; Determines whether a set is valid.
(define (intset-normalized? set)
  (cases set
    [(Num n)         #t]
    [(Range lo hi)   (< lo hi)]
    [(2Sets 1st 2nd) (and (intset-normalized? 1st)
                          (intset-normalized? 2nd)
                          (< (intset-max 1st)
                             (sub1 (intset-min 2nd))))]))

;; Tests
(test (intset-normalized? (Num 1)))
(test (intset-normalized? (Range 1 2)))
(test (not (intset-normalized? (Range 2 1))))
(test (not (intset-normalized? (Range 1 1))))
(test (intset-normalized? (2Sets (Num 1) (Range 3 4))))
(test (not (intset-normalized? (2Sets (Num 1) (Range 2 3)))))
(test (not (intset-normalized? (2Sets (Num 2) (Range 1 3)))))
(test (not (intset-normalized? (2Sets (Range 1 3) (Num 2)))))
(test (intset-normalized? (2Sets (Range 1 2) (Range 4 5))))
(test (not (intset-normalized? (2Sets (Range 1 2) (Range 3 5)))))
(test (not (intset-normalized? (2Sets (Num 3) (Range 1 2)))))
(test (not (intset-normalized? (2Sets (Num 1) (Range 1 2)))))
(test (intset-normalized? (2Sets (Num 1) (Num 3))))
(test (not (intset-normalized? (2Sets (Num 1) (Num 2)))))
(test (intset-normalized? (2Sets (2Sets (Num 1) (Num 3))
                                 (2Sets (Num 5) (Num 7)))))
(test (intset-normalized? (2Sets (2Sets (Range 1 4) (Range 8 10))
                                 (2Sets (Range 12 15)
                                        (2Sets (Num 18) (Num 20))))))

;;; ------------------------------------------------------------------
;;; Question 3

#|

A simple version (it does use ... but only for the number syntax, also
this spells out the syntax for numbers which was not needed)

  <PAGES>   ::= <RANGE> | <RANGE> , <PAGES>
  <RANGE>   ::= <INTEGER> | <INTEGER> - <INTEGER>
  <INTEGER> ::= <NONZERO> <DIGIT> ... | 0
  <NONZERO> ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  <DIGIT>   ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

a version that uses `...' for the ranges, a little more complicated

  <PAGES>   ::= <RANGE> <+RANGE> ...
  <+RANGE>  ::= , <RANGE>
  ...same as above...

and improving the digit non-terminals:

  ...same...
  <NONZERO> ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  <DIGIT>   ::= 0 | <NONZERO>

But you chould just as well skip the whole thing and use `<int>' as
indicated in the homework.  For example:

  <PAGES>   ::= <RANGE> | <RANGE> , <PAGES>
  <RANGE>   ::= <int> | <int> - <int>

Finally, you *could* do it with a single nonterminal by "inlining" <RANGE>:

  <PAGES>   ::= <int> | <int> - <int>
              | <int> , <PAGES> | <int> - <int> , <PAGES>

but that's not good since it repeates the same thing twice.

|#
