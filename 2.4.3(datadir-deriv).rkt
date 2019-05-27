#lang racket
(require "sicp-lang.rkt" racket/trace "util.rkt" "data-structures.rkt")

;;; ex 2.73

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;;;data-directed style of DERIV
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;; sum package
(define (install-sum-package)
  
  (define (make-sum a1 a2) 
    (cond ((=number? a2 0) a1)
          ((=number? a1 0) a2)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (deriv-sum terms var) ;support arbitrarily many terms
    (accumulate
     make-sum
     0
     (map (lambda (term) (deriv term var))
          terms)))
  
  (put 'make '+ make-sum)
  (put 'deriv '+ deriv-sum)
  "sum-package successfully installed")

(define (make-sum a1 a2)
  ((get 'make '+) a1 a2))

;; product
(define (install-product-package)
  
  (define (make-prod a1 a2)
    (cond ((or (=number? a1 0)
               (=number? a2 0))
           0)
          ((=number? a1 1) a2)
          ((=number? a2 1) a1)
          ((and (number? a1)
                (number? a2))
           (* a1 a2))
          (else (list '* a1 a2))))
  
  (define (deriv-prod factors var) ;support arbitrarily many factors
    (if (null? factors)
        0
        (make-sum (accumulate
                   make-prod
                   1
                   (cons (deriv (car factors) var)
                         (cdr factors)))
                  (make-prod
                   (car factors)
                   (deriv-prod (cdr factors)
                               var)))))
    
  (put 'make '* make-prod)
  (put 'deriv '* deriv-prod)
  "product-package successfully installed")

(define (make-product m1 m2)
  ((get 'make '*) m1 m2))


;; exponentiation
(define (install-expt-package)
  
  (define (make-expt b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((=number? b 0) 0)
          ((=number? b 1) 1)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '^ b e))))

  (define (deriv-expt operands var)
    (let ((b (car operands))
          (e (cadr operands)))
      (make-product
       e
       (make-product (make-expt
                      b
                      (make-sum e -1))
                     (deriv b var)))))

  (put 'make '^ make-expt)
  (put 'deriv '^ deriv-expt)
  "exponentiation-package successfully installed")


;;; part 4 of the problem
;; just find all the calls of PUT and exchange the order of their first and second arguments


;;;test
(install-sum-package)
(install-product-package)
(install-expt-package)
(define exp0 '[* 2 x y])
(define exp1 '[* [+ 2 y x] [^ [+ [* y x] 1] 3]])
(define exp2 '[* 2 [^ [+ [* x x y] 3] [+ 2 y]] [+ [* x x] [* 3 x 2] 1]])
(define ddx (lambda [exp] (deriv exp 'x)))
