#lang sicp

(define (GCD a b)
  (if (zero? b)
      a
      (GCD b (remainder a b))))
(define (sign x)
  (cond ((positive? x) 1)
        ((negative? x) -1)
        (else 0)))

(define (make-rat n d)
  (let ((gcd (* (sign d) (abs (GCD n d))))) ;force gcd to have the same sign as d
    (cons (/ n gcd) (/ d gcd))))
(define numer car)
(define denom cdr)

(define (add-rat r1 r2)
  (let ((n1 (numer r1))
        (d1 (denom r1))
        (n2 (numer r2))
        (d2 (denom r2)))
    (make-rat (+ (* n1 d2)
                 (* n2 d1))
              (* d1 d2))))
(define (minus-rat r)
  (make-rat (- (numer r)) (denom r)))
(define (sub-rat r1 r2)
  (add-rat r1 (minus-rat r2)))
(define (mul-rat r1 r2)
  (make-rat (* (numer r1) (numer r2))
            (* (denom r1) (denom r2))))
(define (recip-rat r)
  (make-rat (denom r) (numer r)))
(define (div-rat r1 r2)
  (mul-rat r1 (recip-rat r2)))

(define (print-rat r)
  (newline)
  (display (numer r))
  (display "/")
  (display (denom r)))

;ex 2.1
;my version of make-rat already satisfies this specification