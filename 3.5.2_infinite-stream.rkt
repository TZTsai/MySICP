#lang racket
(require ;"sicp-lang.rkt"
 "util.rkt"
 "3.5.1_stream.rkt")


;; Inifinite stream

(define (integers-start-from n)
  (cons-stream
   n
   (integers-start-from (+ n 1))))

(define natural-numbers
  (integers-start-from 0))



;; generate a stream of primes using the sieve of Eratosthenes

(define (divisible? x n)
  (= (remainder x n) 0))

(define (sieve s)
  (let ([first (stream-car s)])
    (cons-stream
     first
     (sieve (stream-filter
             (lambda (n)
               (not (divisible?
                     n first)))
             (stream-cdr s))))))

(define primes (sieve (integers-start-from 2)))



;; implicitly defining streams

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones
  (cons-stream 1 ones))

(define pos-integers
  (cons-stream 1 (add-streams ones pos-integers)))


(define fibs 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs) fibs))))


(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define primes-implicit
  (cons-stream
   2
   (stream-filter
    prime?
    (integers-start-from 3))))

(define (square n)
  (* n n))

(define (prime? n)
  (define (iter ps)
    (let ([first (stream-car ps)])
      (or (> (square first) n)
          (if (divisible?
               n first)
              false
              (iter (stream-cdr ps))))))
  (iter primes-implicit))

; several utils
(define (neg-stream s)
  (scale-stream s -1))

(define (sub-streams s1 s2)
  (add-streams s1 (neg-stream s2)))



;; Ex 3.53
(define s (cons-stream 1 (add-streams s s)))
; it is the powers of 2


;; Ex 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams pos-integers
                              factorials)))


;; Ex 3.55
(define (partial-sums s)
  (let ([first (stream-car s)])
    (cons-stream first
                 (add-streams
                  (scale-stream ones first)
                  (partial-sums
                   (stream-cdr s))))))

; test
(define triangles (partial-sums natural-numbers))
; (stream-ref triangles 5)


;; Ex 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define S
  (cons-stream 1
               (merge
                (scale-stream S 2)
                (merge
                 (scale-stream S 3)
                 (scale-stream S 5)))))
; the sequence containing exactly all the multiples
; of 2, 3, or 5


;; Ex 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den
           radix)))
; 将num/den展开成以radix为基的小数


;; Ex 3.59
; 1.
(define (integrate-series series)
  (let ([coeffs (stream-map
                 (lambda (n) (/ 1 n))
                 pos-integers)])
    (mul-streams series coeffs)))

; 2.
(define exp-series
  (cons-stream 
   1 (integrate-series exp-series)))

(define sine-series
  (cons-stream
   0 (integrate-series
      cosine-series)))

(define cosine-series
  (cons-stream
   1 (integrate-series
      (stream-map
       (lambda (n) (- n))
       sine-series))))

;; (display-partial-stream sine-series 10)
;; (newline)
;; (display-partial-stream cosine-series 10)

; can I then solve a diffeq?
(define (diff-series series)
  (mul-streams pos-integers
               (stream-cdr series)))

; a proc to solve x = Sum[coeffs{k}*D[x, k], {k, 1, +inf}]
(define (homodiffeq-sol coeffs)
  "?")


;; Ex 3.60
(define (mul-series s1 s2)
  (add-streams
   (scale-stream
    s1
    (stream-car s2))
   (cons-stream
    0 (mul-series
       s1 (stream-cdr s2)))))

; test
(define one
  (add-streams
   (mul-series sine-series
               sine-series)
   (mul-series cosine-series
               cosine-series)))


;; Ex 3.61
(define (invert-unit-series S)
  (define X
    (cons-stream
     1 (neg-stream
        (mul-series
         (stream-cdr S)
         X))))
  X)

; test
;; (define invS (invert-unit-series ones))
;; (display-partial-stream invS 10)

;; the extraordinary thing is that these
;; procs solve the problem merely given
;; the description of the problem
;; I think, usually a program requires
;; practical instructions to solve a problem.
;; Is it a trait possessed by streams?


;; Ex 3.62
(define (div-series s1 s2)
  (let ([den-const (stream-car s2)])
    (if (= 0 den-const)
        (error "den series has zero constant term!")
        (scale-stream
         (mul-series
          s1 (invert-unit-series
              (scale-stream s2 (/ 1 den-const))))
         den-const))))

(define tangent-series
  (div-series sine-series cosine-series))

(display-partial-stream tangent-series 10)

; stream 也太妙了吧
