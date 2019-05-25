#lang sicp
           
;;ex1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n)) ;f(n) = 2n
(define (g n) (A 1 n)) ;g(n) = A(0, A(1, n-1)) = 2A(1, n-1) = ... = 2^(n-1) A(1, 1) = 2^n
(define (h n) (A 2 n)) ;h(n) = A(1, A(2, n-1)) = 2^A(2, n-1) = ... = 2^2^...^2 (2 to the power of 2 for n-1 times)

;;Counting Change
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
;try to figure out a more efficient procedure

;;ex1.11
(define (f-rec n)
  (if (<= n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))
(define (f-iter a b c count)
  (if (= count 0)
      a
      (f-iter b c (+ c (* 2 b) (* 3 a)) (dec count))))
(define (f-it n)
  (f-iter 1 2 3 (- n 1)))

;;ex1.12
(define (pascal m n)
  (cond ((or (< m 1) (< n 1) (> n m)) (error "incorrect numbers"))
        ((or (= n 1) (= n m)) 1)
        (else (+ (pascal (dec m) (dec n))
                 (pascal (dec m) n)))))

;;ex1.16
(define (square x) (* x x))
(define (fast-expt-iter a b n)
  (cond ((zero? n) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (dec n)))))
(define (fast-expt b n)
  (fast-expt-iter 1 b n))

;;ex1.17
(define (fast-mult a b)
  (cond ((zero? b) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (dec b))))))
(define (double x) (* 2 x))
(define (halve x) (/ x 2))

;;ex1.18
(define (fast-mult-iter a b s)
  (cond ((zero? b) s)
        ((even? b) (fast-mult-iter (* 2 a) (/ b 2) s))
        (else (fast-mult-iter a (dec b) (+ s a)))))
(define (fast-mult-it a b)
  (fast-mult-iter a b 0))

;;ex1.19
;find a procedure to compute the Fibonacci numbers in a logarithmic number of steps
;first recall the previous iterative procedure
(define (fib n)
  (define (fib-iter a b cnt)
    (if (zero? cnt)
        a
        (fib-iter b (+ a b) (dec cnt))))
  (fib-iter 0 1 n))
;consider the pair [a b], define T to be the map: [a b]->[b a+b]
;then (fib n) is the car of [a b] after applying T for n times on [0, 1] (which is the idea of fib-iter)
;hence we find a similar pattern to fast-expt-iter and fasst-mult-iter --
;repeating an operation for a number of times on a initial value
;let's imitate the procedures in ex1.16 and ex1.18
(define (fast-fib n)
  ;it turns out that my implementation is very inefficient
  (define (fast-fib-iter pair T cnt)
    (cond ((zero? cnt) (car pair))
          ((even? cnt) (fast-fib-iter pair
                                      (lambda (pair)
                                        (T (T pair)))
                                      (/ cnt 2)))
          (else (fast-fib-iter (T pair) T (dec cnt)))))
  (fast-fib-iter (list 0 1)
                      (lambda (pair)
                        (list (cadr pair)
                              (+ (car pair)
                                 (cadr pair))))
                      n))
;the completion of the template in the textbook is an alternative way
(define (another-fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))    ; compute p'
                   (+ (* 2 p q)
                      (square q))   ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))