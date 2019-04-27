#lang racket
(require "sicp-lang.rkt")
(require "util.rkt")
(provide (all-defined-out))


;;; mathematical procedures

(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a b) 2))

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

(define (enum low high)
  (if (> low high)
      nil
      (cons low 
            (enum 
             (+ low 1) 
             high))))

(define (eval-poly x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))