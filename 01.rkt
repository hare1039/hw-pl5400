;; -------------------------------------------------------------------
;; 1

;; bin4-to-num : BinaryDigit BinaryDigit BinaryDigit BinaryDigit -> Integer
;; Interprets four binary digits as a single number (where BinaryDigit
;; is 0 or 1).
(define (bin4-to-num a b c d)
  (+ a (* 2 b) (* 4 c) (* 8 d)))

#|
;; second version -- a little more uniform, but longer
(define (bin4-to-num a b c d)
  (+ a (* 2 (+ b (* 2 (+ c (* 2 d)))))))

;; third version -- abstracts the above into a helper
(define (bin2 a b) (+ a (* 2 b)))
(define (bin4-to-num a b c d)
  (bin2 a (bin2 b (bin2 c d))))
|#

;; tests
(equal?  0 (bin4-to-num 0 0 0 0))
(equal?  1 (bin4-to-num 1 0 0 0))
(equal? 13 (bin4-to-num 1 0 1 1))
(equal?  8 (bin4-to-num 0 0 0 1))
(equal? 15 (bin4-to-num 1 1 1 1))


;; -------------------------------------------------------------------
;; 2

;; gcd2 : Nonnegative-Integer Nonnegative-Integer -> Nonnegative-Integer
;; Computes the greatest common divisor of two numbers using the
;; binary gcd algorithm.
(define (gcd2 a b)
  (cond [(equal? a 0) b]
        [(equal? b 0) a]
        [(and (even? a) (even? b)) (* 2 (gcd2 (/ a 2) (/ b 2)))]
        [(even? a) (gcd2 (/ a 2) b)]
        [(even? b) (gcd2 a (/ b 2))]
        [(>= a b)  (gcd2 (/ (- a b) 2) b)]
        [else      (gcd2 (/ (- b a) 2) a)]))

;; tests
(equal? 18 (gcd2 378 144))
(equal? 36 (gcd2 216 612))
(equal?  1 (gcd2 999   1))
(equal?  0 (gcd2   0   0))
(equal? 23 (gcd2   0  23))
(equal? 23 (gcd2  23   0))


;; -------------------------------------------------------------------
;; 3

;; all-even? : (Listof Integer) -> Boolean
;; Determines whether the input numbers are all even.
(define (all-even? list)
  (or (null? list)
      (and (even? (first list))
           (all-even? (rest list)))))

;; tests
(all-even? null)
(all-even? '(0))
(all-even? '(2 4 6 8))
(not (all-even? '(1 3 5 7)))
(not (all-even? '(1 3 5 7)))


;; -------------------------------------------------------------------
;; 4

;; merge-lists : (Listof Number) (Listof Number) -> (Listof Number)
;; Merges two sorted lists into a single sorted one.
(define (merge-lists list1 list2)
  (cond [(null? list1) list2]
        [(null? list2) list1]
        [(< (first list1) (first list2))
         (cons (first list1) (merge-lists (rest list1) list2))]
        [else
         (cons (first list2) (merge-lists list1 (rest list2)))]))

;; tests
(equal? null (merge-lists null null))
(equal? '(1 2 3) (merge-lists '(1 2 3) null))
(equal? '(1 2 3) (merge-lists null '(1 2 3)))
(equal? '(1 2 3 4) (merge-lists '(1 2) '(3 4)))
(equal? '(1 2 3 4) (merge-lists '(3 4) '(1 2)))
(equal? '(1 2 3 4) (merge-lists '(1 3) '(2 4)))
(equal? '(1 2 2 3) (merge-lists '(1 3) '(2 2)))
