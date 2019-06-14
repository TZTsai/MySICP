#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;;; My goal in this file is to make the symbolic differentiation system better at
;;; simplifying its results. But to reduce the complexity, I must restrict the input
;;; expression to single variable polynomials. My rule of simplification is:
;;; the result should be in the form of a polynomial of increasing order, which is a 
;;; sum of terms in the form of 
;;;    (make-product <constant> (make-exponentiation <variable> <exponent>)),
;;; except the special cases of the constant being 1 or 0 or the exponent being 1
;;; or 0, which will be simplified respectively


;;; utilities
(define (split lst symbol)
  (define (iter left right)
    (cond ((null? right) #f)
          ((eq? symbol (car right))
           (cons left (cdr right)))
          (else
           (iter (append left (list (car right)))
                 (cdr right)))))
  (iter nil lst))


;;; wishful thinking
(define (deriv exp)
  ;(display "deriv: ") (display exp) (newline)
  (cond ((constant? exp) 0)
        ((variable? exp) 1)
        ((sum? exp)
         (let ((a1 (addend exp))
               (a2 (augend exp)))
           (make-sum (deriv a1)
                     (deriv a2))))
        ((product? exp)
         (let ((m1 (multiplier exp))
               (m2 (multiplicand exp)))
           (make-sum
            (make-product (deriv m1)
                          m2)
            (make-product m1
                          (deriv m2)))))
        ((exponentiation? exp)
         (let ((b (base exp))
               (e (exponent exp)))
           (make-product e
                         (make-product (deriv b)
                                       (make-exponentiation b (make-sum e -1))))))
        (else (error "unknown expression type: DERIV"
                     exp))))


;;;data structures for algebraic expressions

;; a symbol represents a variable
(define variable? symbol?)

;; a number represents a constant
(define constant? number?)
(define (=constant? exp num)
  (and (constant? exp) (= exp num)))

;; a list represents an arithmetic combination
(define combination? list?)


;; sum
(define (addend s)
  (eval (car (split s '+))))
(define (augend s)
  (eval (cdr (split s '+))))
(define (sum? exp)
  (and (combination? exp)
       (split exp '+)))
         
        
(define (make-sum a1 a2)
  ;(print-make "sum" a1 a2)
  (cond ((=constant? a2 0) a1)
        ((=constant? a1 0) a2)
        ((and (constant? a1) (constant? a2))
         (+ a1 a2))
        ((and (variable? a1) (variable? a2))
         (make-product 2 a1))
        ((sum? a1) ;ensure that a1 is not a sum
         (make-sum (addend a1)
                   (make-sum (augend a1) a2))) 
        ((and (sum? a2)
              (not (< (order a1)
                      (order (addend a2))))) ;for sorting the terms
         (make-sum (make-sum (addend a2) a1)
                   (augend a2)))
        ;; below discusses the case of making sum of two terms
        ((= (order a1) (order a2))
         (cond ((and (exponentiation? a1) (product? a2))
                (make-product (make-sum (multiplier a2) 1) a1))
               ((and (variable? a1) (product? a2))
                (make-product (make-sum 1 (multiplier a2))
                              (multiplicand a2)))
               ((and (product? a1) (product? a2))
                (make-product (make-sum (multiplier a1)(multiplier a2))
                              (multiplicand a1)))
               ((and (exponentiation? a1) (exponentiation? a2))
                (make-product 2 a1))
               (else (make-sum a2 a1))))
        ;; so far, a1 and a2 will only be two terms of distinct order
        ((> (order a1) (order a2)) 
         (list a2 '+ a1))
        (else (list a1 '+ a2))))


;; product
(define (multiplier p)
  (eval (car (split p '*))))
(define (multiplicand p)
  (eval (cdr (split p '*))))
(define (product? exp)
  (and (combination? exp)
       (not (sum? exp))
       (split exp '*)))

(define (make-product p1 p2)
  ;(print-make "prod" p1 p2)
  (cond ((or (=constant? p1 0)
             (=constant? p2 0))
         0)
        ((=constant? p1 1) p2)
        ((=constant? p2 1) p1)
        ((and (constant? p1) (constant? p2))
         (* p1 p2))
        ((and (variable? p1) (variable? p2))
         (make-exponentiation p1 2))
        ((sum? p1)
         (make-sum (make-product (addend p1) p2)
                   (make-product (augend p1) p2)))
        ((sum? p2)
         (make-sum (make-product p1 (addend p2))
                   (make-product p1 (augend p2))))
        ;; the 3 cases below discuss the product of two terms without constants
        ((and (variable? p1) (exponentiation? p2))
         (make-exponentiation p1 (make-sum (exponent p2) 1)))
        ((and (exponentiation? p1) (variable? p2))
         (make-exponentiation p2 (make-sum (exponent p1) 1)))
        ((and (exponentiation? p1) (exponentiation? p2))
         (make-exponentiation (base p1)
                              (make-sum (exponent p1)
                                        (exponent p2))))
        ;; then take constants into consideration
        ;; reassociate the factors to simplify further
        ((product? p1) (make-product (multiplier p1)
                                     (make-product p2 (multiplicand p1))))
        ((product? p2) (make-product (make-product p1 (multiplier p2))
                                     (multiplicand p2)))
        ;; so far, a product will only be of a const and a var or an expt
        ((constant? p2) (list p2 '* p1))
        (else (list p1 '* p2))))


;; exponentiation
(define (base expt)
  (eval (car expt)))
(define (exponent expt)
  (eval (caddr expt))) ;the exponent is not allowed to be a var
(define (exponentiation? exp)
  (and (combination? exp)
       (not (sum? exp))
       (not (product? exp))
       (split exp '^)))

(define (make-exponentiation b e)
  ;(print-make "expt" b e)
  (cond ((=constant? e 0) 1)
        ((=constant? e 1) b)
        ((=constant? b 0) 0)
        ((=constant? b 1) 1)
        ((and (constant? b) (constant? e)) (expt b e))
        ((sum? b) (make-product b (make-exponentiation b (- e 1))))
        ((product? b) (make-product (make-exponentiation
                                     (multiplier b) e)
                                    (make-exponentiation
                                     (multiplicand b) e)))
        ((exponentiation? b) (make-exponentiation
                              (base b)
                              (make-product (exponent b) e)))
        (else (list b '^ e))))


;;; other very useful procedures

(define (eval exp)
  ;(display "eval: ") (display exp) (space 2)
  (if (combination? exp)
      (cond ((= (length exp) 1) (eval (car exp)))
            ((sum? exp)
             (make-sum (addend exp) (augend exp)))
            ((product? exp)
             (make-product (multiplier exp) (multiplicand exp)))
            ((exponentiation? exp)
             (make-exponentiation (base exp) (exponent exp)))
            (else (error "unknown expression type: EVAL" exp)))
      exp))

(define (order exp)
  (cond ((constant? exp) 0)
        ((variable? exp) 1)
        ((sum? exp) (max (order (addend exp))
                         (order (augend exp))))
        ((product? exp) (+ (order (multiplier exp))
                           (order (multiplicand exp))))
        ((exponentiation? exp) (exponent exp))
        (else 0)))


;;; convert the given expr into a function of number
(define (expr->func exp)
  (lambda (n)
    (cond ((constant? exp) exp)
          ((variable? exp) n)
          ((sum? exp)
           (+ ((expr->func (addend exp)) n)
              ((expr->func (augend exp)) n))) 
          ((product? exp)
           (* ((expr->func (multiplier exp)) n)
              ((expr->func (multiplicand exp)) n)))
          ((exponentiation? exp)
           (expt ((expr->func (base exp)) n)
                 ((expr->func (exponent exp)) n)))
          (else (error "unknown expression type: NUMVALUE"
                       exp)))))


;;; operations on expressions
(define (add e1 e2)
  (list e1 '+ e2))
(define (mul e1 e2)
  (list e1 '* e2))
(define (sq e)
  (list e '^ 2))
(define (exp e n)
  (list e '^ n))


;;; procedures for printing

;; for debugging
(define (print-make str x y)
  (display str) (display "(") (display x)
  (display ", ") (display y) (display ")")
  (newline))

;; print the returned polynomial neatly
(define (print-poly exp)
  (cond ((constant? exp) (display exp))
        ((variable? exp) (display exp))
        ((exponentiation? exp)
         (for-each display exp))
        ((product? exp)
         (display (multiplier exp))
         (space 1)
         (print-poly (multiplicand exp)))
        ((sum? exp)
         (print-poly (addend exp))
         (display " + ")
         (print-poly (augend exp)))))


;;; for test
(define exp1 '[2 * t + t * t])
(define exp2 '[[2 + t] * [t + 1 + t]])
(define exp3 '[t * t + t])
(define exp4 '[[t + 3 * [t + 2]] ^ 2 + t])
(define exp5 (mul (mul exp1 exp4) (add (sq exp3) (exp exp2 3))))

#| test result:
> (print-poly (deriv exp5))
576 + 10784 t + 63864 t^2 + 184888 t^3 + 304165 t^4 + 303462 t^5 + 187201 t^6 + 69856 t^7 + 14472 t^8 + 1280 t^9
> ((exp->function (deriv exp5)) 3)
513367944
|#



;;; applications

(define (partial-sum var term terms-number)
  (define (iter i result)
    (if (> i terms-number)
        result
        (iter (inc i)
              (add result
                   (mul (term i)
                        (exp var i))))))
  (iter 0 0))

(define (cos-term k)
  (if (odd? k)
      0
      (/ (if (= (remainder k 4) 0)
             1. -1.)
         (factorial k))))

(define cos-series-exp
  (partial-sum 'x cos-term 20))

(define cos
  (expr->func cos-series-exp))

(define minus-sin
  (expr->func (deriv cos-series-exp)))

#| test result:
> (minus-sin (/ PI 2))
-0.9999999999999998
> (cos PI)
-0.9999999999243493
|#

;;; It's just too slow......
;;; no good to use this system to calculate the derivative of a series
;;; its advantage is that it can compute the deriv of a complex form of polynomial
