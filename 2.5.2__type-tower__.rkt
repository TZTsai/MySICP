#lang racket
(require "data-structures.rkt" "math.rkt" "util.rkt"
         (except-in "2.5.1__generic-arithmetic-op__.rkt"
                    apply-generic)
         "2.5.2__coercion__.rkt")
(provide (all-defined-out))


;; Ex 2.83, 2.85
(define (install-type-tower-package)
  (put 'raise '(rational)
       (lambda (r)
         (make-scheme-number (/ (numerator r)
                                (denominator r)))))

  (put 'raise '(scheme-number)
       (lambda (x)
         (make-complex-rect x 0)))

  (put 'droppable? '(rational) (lambda (r) (= (denominator r) 1)))
  (put 'drop '(rational) (lambda (r) (numerator r)))

  (put 'droppable? '(scheme-number) (lambda (n) #f))

  (put 'droppable? '(complex) (lambda (c) (= (imag-part c) 0)))
  (put 'drop '(complex) (lambda (c) (real-part c)))

  (put 'droppable? '(polynomial) (lambda (n) #f)) ;added when I was doing 2.5.3
  'done)

(define (apply-generic-with-tag op . args)
  (letrec ((types (map type-tag args))
           (proc (get op types)))
    (if proc
        (apply proc args)
        (error "Unable to apply" op 'on types))))

(define (raise x)
  (apply-generic-with-tag 'raise x))

(define (drop x)
  (apply-generic-with-tag 'drop x))
(define (droppable? x)
  (and (mpair? x) (apply-generic-with-tag 'droppable? x)))

(define (simplify x)
  (if (droppable? x)
      (simplify (drop x))
      x))

(define (higher-level? x y)
  (define (level-from-top x)
    (with-handlers
      ([exn:fail? (lambda (exn) 0)])
      (+ 1 (level-from-top (raise x)))))
  (let ([x-level (- (level-from-top x))]
        [y-level (- (level-from-top y))])
    (if (> x-level y-level)
        #t #f)))


;; Ex 2.84
(define (apply-generic op . args)
  (define (raise-to-same-level args)
    (define (highest-level args)
      (if (null? (cdr args))
          (car args)
          (if (higher-level? (car args) (highest-level (cdr args)))
              (car args)
              (highest-level (cdr args)))))
    (define (raise-into arg dest-arg)
      (if (higher-level? dest-arg arg)
          (raise-into (raise arg) dest-arg)
          arg))
    (let ((highest (highest-level args)))
      (map (lambda (arg) (raise-into arg highest)) args)))

  (define (try-apply-generic op args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        ""(display op) (display " ")
        ""(display type-tags) (space 4)
        ""(display (get op type-tags)) (newline)
        (if proc
            (apply proc (map contents args))
            'try-apply-generic-failed))))

  (let ((try (try-apply-generic op args)))
    (if (not (eq? try 'try-apply-generic-failed))
        (simplify try)                  ;changed when I do Ex 2.85
        (let ((another-try (try-apply-generic op (raise-to-same-level args))))
          (if (not (eq? another-try 'try-apply-generic-failed))
              (simplify another-try)    ;changed when I do Ex 2.85
              (error "No method for these types"
                     (list op (map type-tag args))))))))


;; Ex 2.85
;; I don't follow the plan given by the exercise. I think explicitly figure
;; out whether a number can be dropped is better than determining a procedure
;; to "project" a number to its lower level and test the droppability by comparing
;; the original number and its projection. Because the "project" procedure is not
;; likely to be used elsewhere.


;; Ex 2.86
;; I think the quickest way is to modify REAL-PART and IMAG-PART, so that
;; they convert the result into scheme-number. The inadequency of this solution
;; is that rational numbers are converted into inaccurate types, but other ways
;; of modification will be much more cumbersome.


(define (type-conversion-update)
  (install-generic-arithmetic-package)
  (install-coercion-package)
  (install-type-tower-package)
  'done
  )

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numerator r)
  (car (contents r)))
(define (denominator r)
  (cdr (contents r)))
(define (make-complex-rect x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-polar r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (print x) (apply-generic 'print x))

(type-conversion-update)