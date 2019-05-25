#lang racket
(require "sicp-lang.rkt")
(require "util.rkt")
(provide (all-defined-out))


;;; mathematical values
(define PI pi)
(define E 2.71828183)

;;; mathematical procedures

(define (square x)
  (* x x))

(define (sq-sum x . y)
  (define (sq-sum-lst l)
    (if (null? l)
        0
        (+ (square (car l))
           (sq-sum-lst (cdr l)))))
  (sq-sum-lst (cons x y)))

(define (average a b)
  (/ (+ a b) 2))

(define (factorial n)
  (define (iter c r)
    (if (> c n)
        r
        (iter (inc c)
              (* c r))))
  (iter 1 1))

(define (prime? n)
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
  (and (> n 1) (= n (smallest-divisor n))))

(define (GCD a b)
  (if (zero? b)
      a
      (GCD b (remainder a b))))

(define (sign x)
  (cond ((positive? x) 1)
        ((negative? x) -1)
        (else 0)))

(define (eval-poly x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))