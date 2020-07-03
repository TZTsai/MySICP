#lang racket
(require
 "util.rkt"
 (except-in "math.rkt" prime?)
 "3.5.1_stream.rkt"
 "3.5.2_infinite-stream.rkt")


(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ([integrand (force delayed-integrand)])
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; (stream-ref (solve (lambda (y) y) 1 0.001) 1000)


;; Ex 3.77
(define (integral* delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ([integrand (force delayed-integrand)])
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (stream-cdr integrand)
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))


;; Ex 3.78
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

;; (stream-ref (solve-2nd 0 -1 1 0 0.001) 3142)


;; Ex 3.79
(define (solve-2nd-generalized f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)


;; Ex 3.80
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC
      (let ([dvC-delayed (delay (scale-stream iL (/ -1 C)))])
        (integral dvC-delayed vC0 dt)))
    (define iL (integral (delay diL) iL0 dt))
    (define diL (sub-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (/ R L))))
    (cons vC iL)))

(define my-RLC-circuit (RLC 1 1 0.2 0.1))
;; (display-partial-stream (cdr (my-RLC-circuit 10 0)) 30)
