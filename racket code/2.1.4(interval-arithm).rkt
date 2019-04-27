#lang sicp

;ex 2.7
(define (make-interval a b) 
  (if (< a b) 
      (cons a b) 
      (cons b a))) 
(define upper cdr) 
(define lower car)
(define lower-bound lower)
(define upper-bound upper)

(define (add-interval x y)
  (make-interval (+ (lower x) (lower y))
                 (+ (upper x) (upper y))))
;ex 2.11
(define (mul-interval x y)
  (let ((x1 (lower x))
        (x2 (upper x))
        (y1 (lower y))
        (y2 (upper y)))
    (let ((x-neg (< x2 0))
          (x-pos (> x1 0))
          (y-neg (< y2 0))
          (y-pos (> y1 0)))
      (cond (x-neg (cond (y-neg (make-interval (* x2 y2) (* x1 y1)))
                         (y-pos (make-interval (* x1 y2) (* x2 y1)))
                         (else (make-interval (* x1 y2) (* x1 y1)))))
            (x-pos (cond (y-neg (make-interval (* x2 y1) (* x1 y2)))
                         (y-pos (make-interval (* x1 y1) (* x2 y2)))
                         (else (make-interval (* x2 y1) (* x2 y2)))))
            (else (cond (y-neg (make-interval (* x2 y1) (* x1 y1)))
                        (y-pos (make-interval (* x1 y2) (* x2 y2)))
                        (else (make-interval (min (* x1 y2) (* x2 y1))
                                             (max (* x1 y1) (* x2 y2))))))))))
(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
;;; test codes
(define (interval-eq? x y)
  (and (= (lower x) (lower y))
       (= (upper x) (upper y))))
(define (interval-generator nlist)
  (if (null? nlist)
      nil
      (append (map (lambda (n) (make-interval (car nlist) n)) (cdr nlist))
              (interval-generator (cdr nlist)))))
(define (pairs l)
  (if (null? l)
      nil
      (append (map (lambda (x) (cons (car l) x)) (cdr l))
              (pairs (cdr l)))))
(define my-intervals (interval-generator (list -1 -5 3 0 9 -7 8)))
(define my-pairs (pairs my-intervals))
(define (results mul pairs)
  (map (lambda (p) (mul (car p) (cdr p))) pairs))
(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      nil
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))
(define test-list
  (let ((result1 (results mul-interval my-pairs))
        (result2 (results old-mul-interval my-pairs)))
    (map (lambda (p) (interval-eq? (car p) (cdr p)))
         (zip result1 result2))))
(define (fold lst comb init)
  (if (null? lst)
      init
      (comb (car lst) (fold (cdr lst) comb init))))
(define final-result (fold test-list (lambda (x y) (and x y)) true))
;> final-result
;#t
;test passed!

;;;"Ben Bitdiddle, an expert systems programmer"......信了你的邪

;ex 2.10
(define (div-interval x y)
  (let ((y1 (upper y))
        (y2 (lower y)))
    (if (<= (* y1 y2) 0)
        (error "Division error (interval spans 0)" y)
        (mul-interval x 
                      (make-interval (/ 1.0 y1)
                                     (/ 1.0 y2))))))
  
;ex 2.8
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper y))
                               (- (lower y)))))

;ex 2.9
(define (interval-width itv)
  (- (upper itv) (lower itv)))
#| For add-interval and sub-interval, the width of the result interval is just the sum of the argument intervals.
Suppose x = [a,b] and y = [c,d], then x+y = [a+c,b+d]. The width of [a+c,b+d] is (b+d)-(a+c) = (b-a)+(d-c), which equals
the sum of the widths of x and y. Since the difference of the two intervals is just the sum of the first interval and the
negative of the second interval, whose width equals the width of the second interval, then the width of the difference is
still the sum of the widths of the two arguments. However, suppose x = [1,2] and y = [1,2], then the width of the product
of x and y: [1,4] is 3. Suppose instead x = [0,1] and y=[0,1], then their product is [0,1] and its width is only 1. Hence
the width of the result of mul-interval and div-interval is not merely dependent on the widths of the arguments.|#

;ex 2.10
;I think that if the interval is used as a representation of an inexact quantity, then this interval is not likely to span zero.
;But still, there may be such cases like temperature, pressure, etc.

;ex 2.12
(define (make-center-percent c p)
  (make-interval (* c (- 1 (* 0.01 p))) (* c (+ 1 (* 0.01 p)))))
(define (center i)
  (/ (+ (lower i) (upper i)) 2))
(define (percent i)
  (let ((c (center i)))
    (* 100 (/ (- (upper i) c) c))))

;ex 2.13
#|
I1 = c1(1 +- 0.01p1), I2 = c2(1 +- 0.01p2)
I1*I2 = [c1c2(1 - 0.01p1)(1 - 0.01p2), c1c2(1+0.01p1)(1+0.01p2)]
its center is c = c1c2(1 + 1e-4p1p2) ~= c1c2
its percentage tolerance is c1c2(p1 + p2)/c ~= p1 + p2
|#

;ex 2.14-2.15
;These two questions as well as ex 2.16 are about the independency of equivalent intervals.
;For example, if A = [1,2], B = [1,2], then A/A should be [1,1] but A/B should be [0.5,2].

;The key point is that if two intervals are dependent, then for any x in the first interval,
;the value of the corresponding y in the second interval is determined. While if the two intervals are
;independent, then x can be any value in the first interval and y can be any value in the second interval.
;For ex 2.15, since par1 refers to both r1 and r2 twice, considering the four intervals in the body as
;independent of each other, the uncertainty is enlarged. While par2 refers to r1 and r2 for only once,
;it then gives the proper uncertainty.