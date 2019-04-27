#lang sicp
(#%require "math.rkt")

;;; data tagging

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

;;; different representations of complex numbers

;; rectangular
(define (make-complex-rect re im)
  (attach-tag 'rect (cons re im)))

(define (rect? complex)
  (equal? (type-tag complex)
          'rect))

;; polar
(define (make-complex-pol radi arg)
  (attach-tag 'pol (cons radi arg)))

(define (pol? complex)
  (equal? (type-tag complex)
          'pol))

;; converting procedures
(define (rect->pol complex)
  (make-complex-pol (mag complex)
                    (arg complex)))

(define (pol->rect complex)
  (make-complex-rect (real complex)
                     (imag complex)))

;; selectors
(define (real z)
  (cond ((rect? z)
         (car (contents z)))
        ((pol? z)
         (* (mag z)
            (cos (arg z))))
        (else
         (error "Unknown type: REAL"
                z))))

(define (imag z)
  (cond ((rect? z)
         (cdr (contents z)))
        ((pol? z)
         (* (mag z)
            (sin (arg z))))
        (else
         (error "Unknown type: IMAG"
                z))))

(define (mag z)
  (cond ((pol? z)
         (car (contents z)))
        ((rect? z)
         (sqrt
          (sq-sum (real z)
                  (imag z))))
        (else
         (error "Unknown type: MAG"
                z))))

(define (arg z)
  (cond ((pol? z)
         (cdr (contents z)))
        ((rect? z)
         (atan
          (/ (imag z)
             (real z))))
        (else
         (error "Unknown type: ARG"
                z))))

;;;;;abstraction barrier;;;;;

(define (add-complex z w)
  (make-complex-rect
   (+ (real z) (real w))
   (+ (imag z) (imag w))))

(define (sub-complex z w)
  (make-complex-rect
   (- (real z) (real w))
   (- (imag z) (imag w))))

(define (mul-complex z w)
  (make-complex-pol
   (* (mag z) (mag w))
   (+ (arg z) (arg w))))

(define (div-complex z w)
  (make-complex-pol
   (/ (mag z) (mag w))
   (- (arg z) (arg w))))