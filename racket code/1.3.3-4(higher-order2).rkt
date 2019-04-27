#lang sicp
(#%require sicp-pict)

;some auxiliary definitions
(define (square x)
  (* x x))
(define (close-enough? a b tolerance)
  (< (abs (- a b)) tolerance))
(define (average a b)
  (* (+ a b) 0.5))
(define (avg-damp f x)
  (average x (f x)))

(define (findroot-half-interval f a b)
  (define tolerance 0.00000001)
  (define (search f neg-pt pos-pt)
    (let ((mid-pt (average neg-pt pos-pt)))
      (cond ((close-enough? neg-pt pos-pt tolerance) mid-pt)
            ((positive? (f mid-pt)) (search f neg-pt mid-pt))
            ((negative? (f mid-pt)) (search f mid-pt pos-pt))
            (else mid-pt))))
  (let ((f1 (f a)) (f2 (f b)))
    (cond ((zero? f1) a)
          ((zero? f2) b)
          ((and (> f1 0) (< f2 0)) (search f b a))
          ((and (< f1 0) (> f2 0)) (search f a b))
          (else (error "Values are not of opposite sign" a b)))))

(define pi (findroot-half-interval sin 3.0 3.2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.000001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fp x)
  (define (f y)
    (/ x y))
  (define (damped y)
    (avg-damp f y))
  (fixed-point damped 1.0))

;ex 1.35
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

;ex 1.36
(define (fixed-point-with-display f init)
  (define tolerance 0.000001)
  (define (iter a b)
    (if (close-enough? a b tolerance)
        b
        (begin (display b)
               (newline)
               (iter b (f b)))))
  (iter init (f init)))

(define (f x)
  (/ (log 1000)
     (log x)))
(define (root)
  (fixed-point-with-display f 5.))
(define (qroot)
  (define (damped x) 
    (avg-damp f x))
  (fixed-point-with-display damped 5.))

;ex 1.37
(define (cont-frac-rec n d k)
  (define (ascend f)
    (lambda (n)
      (f (inc n))))`
  (if (= k 0)
      0
      (/ (n 1) (+ (d 1)
                  (cont-frac-rec (ascend n)
                                 (ascend d)
                                 (dec k))))))

(define (cont-frac-backrec n d k)
  (define (recur i)
    (let ((ni (n i))
          (di (d i)))
      (if (> i k)
          0
          (/ ni (+ di (recur (inc i)))))))
  (recur 1.0))

(define (cont-frac-it n d k)
  (define (iter i frac)
    (if (= i 0)
        frac
        (iter (dec i)
              (/ (n i)
                 (+ (d i) frac)))))
  (iter k 0.0))
;this actually do the iteration backwards
;does the foreward iterative procedure exist?
;consider whether we can find a procedure to transform a k-term cont-frac into k+1-term (given Nk+1 and Dk+1)
;it seems to be impossible, because we only know the result of the k-term cont-frac when doing iteration
;these three procedures all require the number of terms k to be known
;but if we don't know k, instead we have a desired tolerance, like fixed-point
;can we design a corresponding procedure?
;obviously the backward iteration or recursion will not do the job

;ex 1.38
(define (e-cf k)
  (define (n i) 1)
  (define (d i)
    (if (= (remainder i 3) 2)
        (* 2 (+ 1 (quotient i 3)))
        1))
  (+ 2. (cont-frac-it n d k)))

;ex 1.39
(define (tan-cf x k)
  (define (d i)
    (- (* 2 i) 1))
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (cont-frac-it n d k))

;section 1.3.4
;using the idea of procedures as returned values
;we create a procedure whose argument is a function and returns another function
;which is the over-damped version of the original function
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(define (cbrt x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;Newton's method
(define (deriv f)
  (lambda (x)
    (let ((dx 0.000001))
      (/ (- (f (+ x dx))
            (f x))
         dx))))
(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x) ((deriv f) x)))))
(define (newton-method f guess)
  (fixed-point (newton-transform f) guess))

;ex 1.40
(define (cube x)
  (* x x x))
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;ex 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

;ex 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;ex 1.43
;we can see the common pattern in fast-expt, fast-mult and my version of repeated
;let's extract this pattern
(define (fast-iter cnt comb item init)
  (cond ((zero? cnt) init)
        ((even? cnt) (fast-iter (/ cnt 2)
                                comb
                                (comb item item)
                                init))
        (else (fast-iter (dec cnt) comb item (comb item init)))))

(define (fast-expt b n)
  (fast-iter n * b 1))
(define (repeated f n)
  (fast-iter n compose f identity))
;I wrote another better definition in ex1.45.scm

;ex 1.44
(define (smooth f)
  (define (average a b c)
    (/ (+ a b c) 3))
  (let ((dx 0.01))
    (lambda (x)
      (average (f (- x dx))
               (f x)
               (f (+ x dx))))))
(define (n-fold-smooth f n)
  ((repeated smooth n)) f)

(define (data-table f min max dx)
  (define (iter x cnt)
    (define (data)
      (begin (display x)
             (display ": ")
             (display (f x))))
    (define (space)
      (if (zero? (remainder cnt 15))
          (newline)
          (display "   ")))
    (cond ((> x max) (newline))
          (else (data)
                (space)
                (iter (+ x dx) (inc cnt)))))   
  (iter min 1))

;ex 1.45
;I created another file to do this single exercise

;ex 1.46
(define (iter-improve improve good-enough?)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)
(define (simple-sqrt x)
  (define (f y)
    (/ x y))
  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       (* 0.00001 x)))
  ((iter-improve (average-damp f) good-enough?) x))
(define (simple-fixed-point f init)
  ((iter-improve f
                 (lambda (x)
                   (< (abs (- x (f x))) 0.00000001)))
   init))