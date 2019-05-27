#lang racket
(require "util.rkt" "math.rkt" "data-structures.rkt"
         (except-in "2.5.1__generic-arithmetic-op__.rkt"
                    apply-generic install-generic-arithmetic-package))
(provide install-coercion-package ->scheme-number)



;; implement coercion
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-rect (contents n) 0))
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (define (rational->scheme-number n)
    (let ((val (contents n)))
      (/ (numerator val)
         (denominator val))))
  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  (put-coercion 'rational 'scheme-number rational->scheme-number)
  'done)


;; API
(define (install-generic-arithmetic-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-coercion-package)
  'done
  )

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (->scheme-number n)
  (let ((coerce (get-coercion (type-tag n) 'scheme-number)))
    (if coerce
        (coerce n)
        (error "No method for this type -- ->SCHEME-NUMBER"
               (type-tag n)))))


;; EX 2.81

;; 1. the program will fall into infinite loop

;; 2. There is no need for Louis's modification, for if APPLY-GENERIC is called
;; with two arguments of the same type for an operation that is not found in the
;; table for those types, then the program should directly return an error. The
;; program as is manages to do this correctly. Coercion between the same type will
;; cause infinite recursion.

;; 3. Done as below.
;; (define (apply-generic op . args)
;;   (define (no-method type-tags)
;;     (error "No method for these types"
;;            (list op type-tags)))
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (if (= (length args) 2)
;;               (let ((type1 (car type-tags))
;;                     (type2 (cadr type-tags))
;;                     (a1 (car args))
;;                     (a2 (cadr args)))
;;                 (if (equal? type1 type2)
;;                     (no-method type-tags)
;;                     (let ((t1->t2 (get-coercion type1 type2))
;;                           (t2->t1 (get-coercion type2 type1)))
;;                       (cond (t1->t2
;;                              (apply-generic op (t1->t2 a1) a2))
;;                             (t2->t1
;;                              (apply-generic op a1 (t2->t1 a2)))
;;                             (else
;;                              (no-method type-tags))))))
;;                 (no-method type-tags))))))


;; EX 2.82
;; First I give an expample of a situation where this strategy is not sufficiently
;; general: consider a precedure EXP, which takes a geneic number type as its first
;; argument and an integer type as its second and returns the corresponding
;; exponentiation; then if APPLY-GENERIC is called with a complex number and a
;; rational number whose denominator is 1, it cannot coerce the arguments into the
;; right types.


;; the implementation of this strategy
(define (apply-generic op . args)
  (define (coerce-all-into args type)
    (define (coerce-into arg)
      (let ((this-type (type-tag arg)))
        (let ((coerce (if (equal? this-type type)
                          identity
                          (get-coercion this-type type))))
          (if coerce (coerce arg) #f))))
    (let ((coerced-args (map coerce-into args)))
      (if (and-list coerced-args)
          coerced-args
          #f)))
  (define (find-available-coercion type-tags)
    (if (null? type-tags)
        (error "No method for these types"
               (list op type-tags))
        (let ((coerced-args (coerce-all-into args (car type-tags))))
          (if coerced-args
              (let ((proc (get op (map type-tag coerced-args))))
                (if proc
                    (apply proc (map contents coerced-args))
                    (find-available-coercion (cdr type-tags))))
              (find-available-coercion (cdr type-tags))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (find-available-coercion (map type-tag args))))))

;; for test
(define (test)
  (install-generic-arithmetic-package)
  )
