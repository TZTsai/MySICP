#lang sicp


;;;Now I have only finished part of it. Moreover, there are some bugs in this program.

;;;Alert! I only considered the case of single variable and constant exponent in my make-sum, make-product and make-exponentiation


;;;wishful thinking
(define (deriv exp var)
  (cond ((constant? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (let ((a1 (eval (addend exp)))
               (a2 (eval (augend exp)))) 
           (make-sum (deriv a1 var)
                     (deriv a2 var))))
        ((product? exp)
         (let ((m1 (eval (multiplier exp)))
               (m2 (eval (multiplicand exp))))
           (make-sum
            (make-product (deriv m1 var)
                          m2)
            (make-product m1
                          (deriv m2 var)))))
        ((exponentiation? exp)
         (let ((b (eval (base exp)))
               (e (eval (exponent exp))))
           (make-product e
                         (make-product (make-exponentiation b (make-sum e -1))
                                       (deriv b var)))))
        (else (error "unknown expression type: DERIV"
                     exp))))

;;;data structures for algebraic expressions

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;a symbol represents a variable
(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;a number represents a constant
(define constant? number?)

;determine whether an expression is a combination
(define combination? pair?)

;make some useful procedures
#|
(define (is-comb-of? op)
  (lambda (exp)
    (and (combination? exp)
         (eq? (car exp) op))))
|#

(define (is-comb-of? op)
  (lambda (exp)
    (and (combination? exp)
         (eq? (cadr exp) op))))

;use a list the car of which is '+ to represent a sum

#|
(define (addend sum)
  (cadr sum))
(define (augend sum)
  (accumulate make-sum 0 (cddr sum)))
(define sum? (is-comb-of? '+))
|#
;ex 2.58: infix representation
(define (addend sum)
  (car sum))
(define (augend sum)
  (eval (cddr sum)))
(define sum? (is-comb-of? '+))

(define (make-sum a1 a2)
  (cond ((=number? a2 0) a1)
        ((=number? a1 0) a2)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((same-variable? a1 a2) (make-product 2 a1))                              
        ;(else (list '+ a1 a2))))
        (else (list a1 '+ a2))))

;similarly for a product

#|
(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (accumulate make-product 1 (cddr p)))
(define product? (is-comb-of? '*))
|#
;ex 2.58
(define (multiplier p)
  (car p))
(define (multiplicand p)
  (eval (cddr p)))
(define product? (is-comb-of? '*))

(define (make-product p1 p2)
  (cond ((or (=number? p1 0)
             (=number? p2 0))
         0)
        ((=number? p1 1) p2)
        ((=number? p2 1) p1)
        ((and (number? p1) (number? p2))
         (* p1 p2))
        ((number? p2) (make-product p2 p1))
        ((same-variable? p1 p2) (make-exponentiation p1 2))
        ((sum? p1) (make-sum (make-product (addend p1) p2)
                             (make-product (augend p1) p2)))
        ((sum? p2) (make-sum (make-product p1 (addend p2))
                             (make-product p1 (augend p2))))
         ;(else (list '* p1 p2))))
        (else (list p1 '* p2))))

;for exponentiation

#|
(define (base expt) (cadr expt))
(define (exponent expt) (caddr expt))
(define exponentiation? (is-comb-of? '^))
|#
;ex 2.58
(define (base expt) (car expt))
(define (exponent expt) (eval (cddr expt)))
(define exponentiation? (is-comb-of? '^))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 0) 0)
        ((=number? b 1) 1)
        ((and (number? b) (number? e)) (expt b e))
        ;(else (list '^ b e))))
        (else (list b '^ e))))


;;;evaluate before combination to simplify the answer
#|
(define (eval exp)
  (cond ((sum? exp) (make-sum (eval (addend exp)) (eval (augend exp))))
        ((product? exp) (make-product (eval (multiplier exp)) (eval (multiplicand exp))))
        ((exponentiation? exp) (make-exponentiation (eval (base exp)) (eval (exponent exp))))
        (else exp)))
|#
(define (eval exp)
  (if (combination? exp)
      (cond ((= (length exp) 1) (eval (car exp)))
            ((sum? exp) (make-sum (eval (addend exp)) (eval (augend exp))))
            ((product? exp) (make-product (eval (multiplier exp)) (eval (multiplicand exp))))
            ((exponentiation? exp) (make-exponentiation (eval (base exp)) (eval (exponent exp))))
            (else (error "unknown expression type: EVAL" exp)))
      exp))

;; prefix->infix
(define (comb-of? op)
  (lambda (exp)
    (and (combination? exp)
         (eq? (car exp) op))))
(define (comb op)
  (define (operate args)
    (if (null? (cdr args))
        (prefix->infix (car args))
        (list (prefix->infix (car args))
              op
              (operate (cdr args)))))
  operate)

(define (prefix->infix exp)

  (define sum? (comb-of? '+))
  (define add (comb '+))

  (define prod? (comb-of? '*))
  (define mul (comb '*))

  (define expt? (comb-of? '^))
  (define expt (comb '^))

  (if (not (combination? exp))
      exp
      (cond ((sum? exp) (prefix->infix (add (cdr exp))))
            ((prod? exp) (prefix->infix (mul (cdr exp))))
            ((expt? exp) (prefix->infix (expt (cdr exp))))
            (else exp))))

;;;test
(define prefix-exp '(+ (^ (+ (* y x) 1) 3) (* (+ 2 y x) (* 3 (* (^ (+ (* y x) 1) 2) y)))))
(define infix-exp '[[2 + y + x] * [[y * x + 1] ^ 3]])
(define infix-exp2 '[2 * [[x * x * y + 3] ^ [2 * y + 1]] * [x * x + 3 * x * 2 + 1]])