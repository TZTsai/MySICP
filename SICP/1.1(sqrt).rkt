#lang sicp

(define (square x) (* x x))
(define (sum-of-squares a b)
  (+ (square a) (square b)))
;;ex1.3
(define (sqsum-of-two-larger a b c)
  (cond ((and (>= a b) (>= b c)) (sum-of-squares a b))
        ((>= a b) (sum-of-squares a c))
        (else (sum-of-squares b c))))

;;ex1.5
(define (et) (et))
(define (test x y) ;test whether the interpreter evaluates in an applicative-order way or a normal-order way
  (if (= x 0)
      x
      y))

;;Newton's method to calculate square root
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-iter guess x)
  (define (good-enough? guess)
    (< (abs (- (square guess)
               x))
       0.0001))
  
  ;;ex1.7 begin
  ;A guess is good enough when: 
  ;  abs(improved-guess - original-guess) / original-guess < 0.0001 
  (define (better-good-enough? guess)
    (< (/ (abs (- guess
                  (improve guess)))
          guess)
       0.0001))
  ;An alternative test
  (define (yet-another-good-enough? guess)
    (< (/ (abs (- (square guess) x))
          x)
       0.0001))
  ;;ex1.7 end
  
  (define (improve guess)
    (average (/ x guess) guess))
  
  (if (better-good-enough? guess) ;change good-enough? in this line
      guess
      (sqrt-iter (improve guess)
                 x)))
  ;try replacing good-enough? with better-good-enough? to see the improvement
  ;good-enough? works terrible for both tiny and large numbers but better-good-enough? works well for them
  ;however, it turns out that for moderately big number, this test is less precise than the original one

(define (sqrt x)
  (sqrt-iter 1.0 x))

;test the precisions of several kinds of good-enough?
(define (test-precision n)
  (* 100 (abs (/ (- (square (sqrt n))
                    n)
                 n))))

;;ex1.8
(define (cbrt-iter guess x)
  (define (good-enough? guess)
    (< (abs (/ (- (square guess) x)
               x))
       0.0001))
  (define (improve guess)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (if (good-enough? guess)
      guess
      (cbrt-iter (improve guess)
                 x)))
(define (cbrt x)
  (cbrt-iter 1.0 x))
;we can see much common pattern between the implementations of sqrt and cbrt
;further abstraction can be made