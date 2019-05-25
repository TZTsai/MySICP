#lang sicp

(define (cons* x y)
  (lambda (n)
    (if (= n 0)
        x
        y)))
(define (car* p)
  (p 0))
(define (cdr* p)
  (p 1))
(define (list3 a b c)
  (cons* a (cons* b (cons* c nil))))

;ex 2.4
(define (cons** x y)
  (lambda (m) (m x y)))
(define (car** z)
  (z (lambda (p q) p)))
(define (cdr** z)
  (z (lambda (p q) q)))

;ex 2.5
(define (cons*** x y)
  (* (expt 2 x) (expt 3 y)))
(define (factor-power p n)
  (if (not (= (remainder n p) 0))
      0
      (inc (factor-power p (/ n p)))))
(define (car*** z)
  (factor-power 2 z))
(define (cdr*** z)
  (factor-power 3 z))

;ex 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) f))
(define two
  (lambda (f) (lambda (x) (f (f x)))))
(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
(define (church-to-int church)
  ((church inc) 0))