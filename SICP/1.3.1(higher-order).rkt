#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (square x) (* x x))
(define (cube x) (* x x x))

;ex1.29
(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n))
        (fixed-n (+ n (remainder n 2))))
    (define (term k)
      (define y (f (+ a (* k h))))
      (cond ((or (= k 0) (= k fixed-n)) y)
            ((even? k) (* 2 y))
            (else (* 4 y))))
    (* (/ h 3) (sum term 0 inc fixed-n))))

;ex1.30
(define (sum-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;ex1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
(define (factorial n)
  (product identity 1 inc n))
(define (pi-product n)
  (define (term k)
    (/ (* (* 2 k) (+ (* 2 k) 2))
       (square (+ (* 2 k) 1))))
  (* 4 (product term 1. inc n)))

;ex1.32
(define (accumulate comb init-val term a next b)
  (if (> a b)
      init-val
      (comb (term a)
            (accumulate comb init-val term (next a) next b))))
(define (accumulate-it comb init-val term a next b)
  (define (iter k result)
    (if (> k b)
        result
        (iter (next k) (comb (term k) result))))
  (iter a init-val))
(define (sum-acc term a next b)
  (accumulate + 0 term a next b))
(define (product-acc term a next b)
  (accumulate * 1 term a next b))

;ex1.33
(define (filtered-accumulate pred comb null-val term a next b)
  (define (filtered-term k)
    (if (pred k) (term k) null-val))
  (accumulate comb null-val filtered-term a next b))

;some auxiliary definitions
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (define (next k)
    (if (= k 2) 3 (+ k 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (and (> n 1) (= n (smallest-divisor n))))
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

;ex1.33 a.
(define (sqsum-of-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))
;ex1.33 b.
(define (product-relatively-prime n)
  (define (relatively-prime? k)
    (= (gcd k n) 1))
  (filtered-accumulate relatively-prime? * 1 identity 1 inc (- n 1)))