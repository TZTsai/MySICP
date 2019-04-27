#lang sicp
(#%require (lib "27.ss" "srfi"))

(define (square x)
  (* x x))
;primality test by finding the smallest divisor
(define (smallest-divisor-old n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;fermat test
(define (expmod base exp m)
  (cond ((zero? exp) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base
                       (expmod base (dec exp) m))
                       m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;ex1.22
;provided by textbook
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " time cost: ")
  (display elapsed-time))

;;;my own timer
(define (timer f)
    (lambda (n)
      (start f n (runtime))))
(define (start f n start-time)
  (f n)
  ;(display (f n)) ;display the computed result
  ;(newline)
  (display "elapsed time: ")
  (display (- (runtime) start-time)))

;find the next prime and display the time cost
(define (next-prime start)
  (define (search-iter n)
    (if (and (odd? n) (prime? n))
        (timed-prime-test n)
        (search-iter (inc n))))
  (search-iter start))

;;;;; output
; > (next-prime 1000000000)
; 1000000007 *** 2478
; > (next-prime 10000000000)
; 10000000019 *** 7459
; > (next-prime 100000000000)
; 100000000003 *** 24801
; > (next-prime 1000000000000)
; 100000000003 *** 81971
; > (next-prime 10000000000000)
; 10000000000037 *** 286438
;;;;;
;almost coincides with the Theta(sqrt(n)) growth

;ex1.23
(define (smallest-divisor n)
  (quicker-find-divisor n 2))
(define (quicker-find-divisor n test-divisor)
  (define (next div)
    (if (= div 2)
        3
        (+ div 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
;using the tests in ex1.22
;I found that the time costs are almost the same, with a little acceleration

;ex1.24
(define (fast-next-prime start)
  (define (search-iter n)
    (if (and (odd? n) (fast-prime? n 100))
        (fast-timed-prime-test n)
        (search-iter (inc n))))
  (search-iter start))
(define (fast-timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (fast-prime? n 100)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " time cost: ")
    (display elapsed-time))
  (newline)
  (display n)
  (start-prime-test n (runtime)))
;the time cost is very irregular
;that of larger numbers can be even smaller than relatively small numbers
;maybe there is something wrong

;ex1.25
(define (fast-expt-iter a b n)
  (cond ((zero? n) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (dec n)))))
(define (fast-expt b n)
  (fast-expt-iter 1 b n))
(define (test1 n)
  (define (ok n)
    (expmod 23 n n))
  ((timer ok) n))
(define (test2 n)
  (define (alyssa n)
    (remainder (fast-expt 23 n) n))
  ((timer alyssa) n))

;ex1.27
(define (full-fermat-test n)
  (define (loop i)
    (cond ((= i n) true)
          ((not (= i (expmod i n n)))
           false)
          (else (loop (inc i)))))
  (loop 1))
; > (map full-fermat-test (list 561 1105 1729 2465 2821 6601))
; (mcons #t (mcons #t (mcons #t (mcons #t (mcons #t (mcons #t '()))))))

;ex1.28
(define (mr-test n)
  (define (try-it a) 
    (define (check-it x) 
      (and (not (= x 0)) (= x 1))) 
    (check-it (mr-expmod a (- n 1) n))) 
  (try-it (+ 1 (random (- n 1)))))
(define (mr-expmod b e m)
  (define (check-square-mod n)
    (define (check-non-trivial-sqrt-eq1 n sqmod)
      (if (and (= sqmod 1)
                (not (= n 1))
                (not (= n (dec m))))
           0
           sqmod))
    (check-non-trivial-sqrt-eq1 n (remainder (square n) m)))
  (cond ((zero? e) 1)
        ((even? e)
         (check-square-mod (mr-expmod b (/ e 2) m)))
        (else
         (remainder (* b
                       (expmod b (dec e) m))
                    m))))
(define (mr-prime? n times)
  (define (loop t)
    (cond ((> t times) true)
          ((mr-test n) (loop (inc t)))
          (else false)))
  (loop 1))