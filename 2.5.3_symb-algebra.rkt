#lang sicp
(#%require "compatible.rkt" "util.rkt" "math.rkt" "data-structures.rkt"
           (only racket with-handlers exn:fail? letrec))
(#%provide (all-defined))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (conjugate z)
    (make-from-real-imag
     (real-part z)
     (- (imag-part z))))
  (define (equ? z1 z2)
    (and (eq? (real-part z1)
              (real-part z2))
         (eq? (imag-part z1)
              (imag-part z2))))
  (define (=zero? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))
  (define (print-rect z)
    (print (real-part z))
    (let ([imag (imag-part z)])
      (if (> imag 0)
          (display "+")
          (display ""))
      (print imag))
    (display "i"))

  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'conjugate '(rectangular) conjugate)
  (put 'equ? '(rectangular rectangular) equ?)
  (put '=zero? '(rectangular) =zero?)
  (put 'print '(rectangular) print-rect)
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a)
    (if (< r 0)
        (cons (- r) (- a))
        (cons r a)))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (conjugate z)
    (make-from-mag-ang
     (magnitude z)
     (- (angle z))))
  (define (equ? z1 z2)
    (and (eq? (magnitude z1)
              (magnitude z2))
         (integer? (/ (- (angle z1) (angle z2))
                      (* 2 PI)))))
  (define (=zero? z)
    (= (magnitude z) 0))
  (define (print-polar z)
    (print (magnitude z))
    (display "e^")
    (print (angle z))
    (display "i"))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'conjugate '(polar) conjugate)
  (put 'equ? '(polar polar) equ?)
  (put '=zero? '(polar) =zero?)
  (put 'print '(polar) print-polar)
  'done)


;;; upper level packages
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (make-scheme-number (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (make-scheme-number (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (make-scheme-number (* x y))))
  (put 'div '(scheme-number scheme-number)
      (lambda (x y) (make-scheme-number (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) eq?)
  (put '=zero? '(scheme-number) zero?)
  (put 'print '(scheme-number) display)
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (= d 0)
        (error "divided by zero: MAKE-RAT" n d)
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (eq-rat? x y)
    (and (eq? (numer x) (numer y))
         (eq? (denom x) (denom y))))
  (define (=zero? r)
    (= (numer r) 0))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) eq-rat?)
  (put '=zero? '(rational) =zero?)
  (put 'print '(rational)
       (lambda (r)
         (display (/ (numer r)
                     (denom r)))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang
          'polar)
     r a))
  ;; internal procedures
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  (define (conjugate z)
    (apply-generic 'conjugate z))

  (define (add-complex z1 z2)
    (make-from-real-imag
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (sub (real-part z1) (real-part z2))
     (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (mul (magnitude z1) (magnitude z2))
     (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (div (magnitude z1) (magnitude z2))
     (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'real-part 'complex (lambda (z) (apply-generic 'real-part z)))
  (put 'imag-part 'complex (lambda (z) (apply-generic 'imag-part z)))
  (put 'magnitude 'complex (lambda (z) (apply-generic 'magnitude z)))
  (put 'angle 'complex (lambda (z) (apply-generic 'angle z)))
  (put 'conjugate 'complex (lambda (z) (apply-generic 'conjugate z)))
  (put 'equ? '(complex complex) (lambda (z1 z2) (apply-generic 'equ? z1 z2)))
  (put '=zero? '(complex) (lambda (z) (apply-generic '=zero? z)))
  (put 'print '(complex) (lambda (z) (apply-generic 'print z)))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-dense-poly-package)
  ;; internal definitions
  (define (make-dense-poly var coeff-list)
    (define (del-preceeding-zeros cl)
      (if (and (not (empty?-coefflist cl)) (=zero? (first-coeff cl)))
          (del-preceeding-zeros (cdr cl))
          cl))
    (cons var (del-preceeding-zeros coeff-list)))
  (define (variable p) (car p))
  (define (coeff-list p) (cdr p))
  (define (same-variable? x y)
    (and (symbol? x)
         (symbol? y)
         (eq? x y)))
  (define (=zero?-poly p)
    (all? =zero? (coeff-list p)))

  ;; representation of coeff lists
  (define (the-empty-coefflist) null)
  (define (empty?-coefflist cl) (null? cl))
  (define (order cl) (- (length cl) 1))
  (define (first-coeff cl) (car cl))
  (define (rest-coeffs cl) (cdr cl))

  (define (coeffs-op->poly-op cl-op p1 p2)
      (if (same-variable? (variable p1)
                          (variable p2))
          (make-dense-poly (variable p1)
                           (cl-op (coeff-list p1)
                                  (coeff-list p2)))
          (error "polys not in same var" )))

  (define (add-poly p1 p2)
    (coeffs-op->poly-op add-coeffs p1 p2))

  (define (add-coeffs cl1 cl2)
    (cond [(empty?-coefflist cl1) cl2]
          [(empty?-coefflist cl2) cl1]
          [(> (order cl1) (order cl2))
           (cons (first-coeff cl1)
                 (add-coeffs (rest-coeffs cl1) cl2))]
          [(= (order cl1) (order cl2))
           (cons (add (first-coeff cl1)
                      (first-coeff cl2))
                 (add-coeffs (rest-coeffs cl1)
                                (rest-coeffs cl2)))]
          [else (add-coeffs cl2 cl1)]))

  (define (mul-poly p1 p2)
    (coeffs-op->poly-op mul-coeffs p1 p2))

  (define (scale-coeffs cl c)
    (map (lambda (coeff) (mul c coeff)) cl))

  (define (mul-coeffs cl1 cl2)
    (define (raise-order cl)
      (append cl (list 0)))
    (define (iter cl1 cl2 product)
      (if (or (empty?-coefflist cl1) (empty?-coefflist cl2))
          product
          (iter cl1 (rest-coeffs cl2)
                (add-coeffs (raise-order product)
                               (scale-coeffs cl1 (first-coeff cl2))))))
    (iter cl1 cl2 (the-empty-coefflist)))

  (define (neg-poly p)
    (make-dense-poly (variable p)
                     (map (lambda (c) (mul -1 c))
                          (coeff-list p))))

  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))

  (define (div-poly p1 p2)
    (coeffs-op->poly-op (lambda (cl1 cl2)
                          (car (div-coeffs cl1 cl2)))
                        p1 p2))

  (define (rem-poly p1 p2)
    (coeffs-op->poly-op (lambda (cl1 cl2)
                          (cadr (div-coeffs cl1 cl2)))
                        p1 p2))

  (define (div-coeffs cl1 cl2)
    (define (one-div cl1 cl2)
      (define (sub-head orig-cl mult-cl)
        (if (empty?-coefflist mult-cl)
            orig-cl
            (cons (sub (first-coeff orig-cl)
                       (first-coeff mult-cl))
                  (sub-head (rest-coeffs orig-cl)
                            (rest-coeffs mult-cl)))))
      (letrec ([q (div (first-coeff cl1) (first-coeff cl2))]
               [mult-cl (scale-coeffs cl2 q)])
        (list q (cdr (sub-head cl1 mult-cl)))))

    (define (iter cl1 cl2 q)
      (if (empty?-coefflist cl2)
          (list q (the-empty-coefflist))
          (if (< (order cl1) (order cl2))
              (list q cl1)
              (letrec ([one-div-result (one-div cl1 cl2)]
                       [quo (car one-div-result)]
                       [rem (cadr one-div-result)])
                (iter rem cl2 (append q (list quo)))))))

    (iter cl1 cl2 (the-empty-coefflist)))

  (define (tag poly) (attach-tag 'dense-poly poly))
  (put 'make 'dense-poly (lambda (var cl) (tag (make-dense-poly var cl))))
  (put 'coeff-list 'dense-poly coeff-list)
  (put 'variable '(dense-poly) variable)
  (put '=zero? '(dense-poly) =zero?-poly)
  (put 'add '(dense-poly dense-poly) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(dense-poly dense-poly) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(dense-poly dense-poly) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(dense-poly dense-poly) (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'rem '(dense-poly dense-poly) (lambda (p1 p2) (tag (rem-poly p1 p2))))
  (put 'print '(dense-poly) (lambda (p) (print (dense-poly->poly (tag p)))))
  )

(define (coeff-list denseP)
  ((get 'coeff-list 'dense-poly) (contents denseP)))

(define (dense-poly->poly dp)
  (define (coeffL->termL cL)
    (if (null? cL)
        null
        (adjoin-term (make-term (- (length cL) 1)
                                (car cL))
                     (coeffL->termL (cdr cL)))))
  (make-poly (variable dp)
             (coeffL->termL (coeff-list dp))))


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? x y)
    (and (symbol? x)
         (symbol? y)
         (eq? x y)))
  (define (=zero?-poly p)
    (let ([coeffs (map coeff (term-list p))])
      (display coeffs)
      (all? =zero? coeffs)))

  ;; representation of terms and term lists
  (define (the-empty-termlist) null)
  (define (empty-termlist? tl) (null? tl))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (first-term tl) (car tl))
  (define (rest-terms tl) (cdr tl))
  (define (adjoin-term term tl)
    (if (=zero? (coeff term))
        tl
        (cons term tl)))

  (define (terms-op->poly-op terms-op p1 p2)
      (if (same-variable? (variable p1)
                          (variable p2))
          (make-poly
           (variable p1)
           (terms-op (term-list p1)
                     (term-list p2)))
          (error "Polys not in same var"
                 (list p1 p2))))

  (define (add-poly p1 p2)
    (terms-op->poly-op add-terms p1 p2))

  (define (mul-poly p1 p2)
    (terms-op->poly-op mul-terms p1 p2))

  (define (sub-poly p1 p2)
    (terms-op->poly-op (lambda (L1 L2) (add-terms L1 (neg-terms L2))) p1 p2))

  (define (div-poly p1 p2)
    (terms-op->poly-op (lambda (L1 L2) (car (div-terms L1 L2))) p1 p2))

  (define (rem-poly p1 p2)
    (terms-op->poly-op (lambda (L1 L2) (cadr (div-terms L1 L2))) p1 p2))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1
                     (add-terms (rest-terms L1)
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2
                     (add-terms
                      L1
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term
                      (order t1)
                      (add (coeff t1)
                           (coeff t2)))
                     (add-terms
                      (rest-terms L1)
                      (rest-terms L2)))))))))

  (define (neg-terms L)
    (map (lambda (term)
           (make-term (order term)
                      (mul -1 (coeff term))))
         L))

  (define (mul-terms L1 L2)
    (define (mul-term-by-all-terms t1 L)
      (if (empty-termlist? L)
          (the-empty-termlist)
          (let ((t2 (first-term L)))
            (adjoin-term
             (make-term
              (+ (order t1) (order t2))
              (mul (coeff t1) (coeff t2)))
             (mul-term-by-all-terms
              t1
              (rest-terms L))))))
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms
         (mul-term-by-all-terms
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist)
              (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1)
                                (coeff t2)))
                    (new-o (- (order t1)
                              (order t2))))
                (let ((rest-of-result
                       (div-terms
                        (add-terms L1 (mul-terms L2
                                                 (list (make-term
                                                        new-o
                                                        (mul -1 new-c)))))
                        L2)))
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))

  (define (print-poly poly)
    (define (print-term term var)
      (let ([ord (order term)]
            [cf (coeff term)])
        (if (and (pair? cf) (not (eq? (type-tag cf) 'rational)))
            (bracket (print cf))
            (if (equ? 1 cf)
                (display "")
                (print cf)))
        (cond ((= ord 0) (display ""))
              ((= ord 1) (display var))
              (else (display var)
                    (display "^")
                    (display ord)))))
    (define (print-terms terms var)
      (cond ((null? terms) (display ""))
            ((null? (cdr terms)) (print-term (car terms) var))
            (else (print-term (car terms) var)
                  (display " + ")
                  (print-terms (cdr terms) var))))
    (print-terms (term-list poly) (variable poly)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (div-poly p1 p2))))
  (put 'rem  '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (rem-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  (put 'print '(polynomial) print-poly)
  (put 'variable '(polynomial) variable)
  (put 'make-term 'polynomial make-term)
  (put 'adjoin-term 'polynomial adjoin-term)
  'done)

(define (make-term order coeff)
  ((get 'make-term 'polynomial) order coeff))
(define (adjoin-term term term-list)
  ((get 'adjoin-term 'polynomial) term term-list))


(define (install-type-tower-package)
  (put 'raise '(rational)
       (lambda (r)
         (make-scheme-number (/ (numerator r)
                                (denominator r)))))
  (put 'raise '(scheme-number)
       (lambda (x)
         (make-complex-rect x 0)))
  (put 'raise '(dense-poly) dense-poly->poly)

  (put 'droppable? '(rational) (lambda (r) (= (denominator r) 1)))
  (put 'drop '(rational) (lambda (r) (numerator r)))
  (put 'droppable? '(scheme-number) (lambda (n) #f))
  (put 'droppable? '(complex) (lambda (c) (= (imag-part c) 0)))
  (put 'drop '(complex) (lambda (c) (real-part c)))
  (put 'droppable? '(polynomial) (lambda (n) #f)) ;added when I was doing 2.5.3
  (put 'droppable? '(dense-poly) (lambda (p) #f))
  'done)


;;; API
(define precision 3)

(define (install-generic-arithmetic-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-polynomial-package)
  (install-dense-poly-package)
  (install-type-tower-package)
  'done
  )


(define (make-scheme-number n)
  (let ([10-pow (expt 10 precision)])
    ((get 'make 'scheme-number) (/ (floor (* 10-pow n)) 10-pow))))

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numerator r)
  ((get 'numer 'rational) (contents r)))
(define (denominator r)
  ((get 'denom 'rational) (contents r)))

(define (make-complex-rect x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-polar r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (real-part z)
  ((get 'real-part 'complex) (contents z)))
(define (imag-part z)
  ((get 'imag-part 'complex) (contents z)))
(define (magnitude z)
  ((get 'magnitude 'complex) (contents z)))
(define (angle z)
  ((get 'angle 'complex) (contents z)))
(define (conjugate z)
  ((get 'conjugate 'complex) (contents z)))

(define (make-poly var term-list)
  ((get 'make 'polynomial) var term-list))
(define (make-dense-poly var coeff-list)
  ((get 'make 'dense-poly) var coeff-list))
(define (variable poly)
  (apply-generic 'variable poly))


;; the most big procedure
(define (apply-generic op . args)
  (define (raise-to-same-level args)
    (define (highest-level args)
      (if (null? (cdr args))
          (car args)
          (if (supertype? (car args) (highest-level (cdr args)))
              (car args)
              (highest-level (cdr args)))))
    (define (raise-into arg dest-arg)
      (if (supertype? dest-arg arg)
          (raise-into (raise arg) dest-arg)
          arg))
    (let ((highest (highest-level args)))
      (map (lambda (arg) (raise-into arg highest)) args)))

  (define (try-apply-generic op args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
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
  (and (pair? x) (apply-generic-with-tag 'droppable? x)))

(define (simplify x)
  (if (droppable? x)
      (simplify (drop x))
      x))

(define (supertype? x y)
  (define (level-from-top x)
    (with-handlers
      ([exn:fail? (lambda (exn) 0)])
      (+ 1 (level-from-top (raise x)))))
  (let ([x-level (- (level-from-top x))]
        [y-level (- (level-from-top y))])
    (if (> x-level y-level)
        #t #f)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (rem x y) (apply-generic 'rem x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (print x) (apply-generic 'print x))



(define (test)
  (set! precision 5) ; set the number of precision digits here
  (install-generic-arithmetic-package)
  (letrec ([a (make-rational 1 2)]
           [b (make-rational 6 4)]
           [c (make-complex-rect 3 4)]
           [d (make-complex-polar (sqrt 8) (/ PI 4))]
           [p (make-poly 'x (list (make-term 5 b) (make-term 3 c) (make-term 2 a) (make-term 0 5)))]
           [dp (make-dense-poly 'x (list 1 1 1 1))]
           [dq (make-dense-poly 'x (list 2 2 2 2 2))])
    (display "quotient poly: ")
    (print (div p dp))
    (newline)
    (display "remainder poly: ")
    (print (rem p dp))))
(test)
