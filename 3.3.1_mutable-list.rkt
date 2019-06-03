#lang racket
(provide mcons mcar mcdr set-car! set-cdr! print-list mlist)

(define (mcons x y)
  (lambda (op)
    (cond [(eq? op 'car) x]
          [(eq? op 'cdr) y]
          [(eq? op 'set-car!)
           (lambda (z)
             (set! x z))]
          [(eq? op 'set-cdr!)
           (lambda (z)
             (set! y z))])))

(define (mcar p)
  (p 'car))
(define (mcdr p)
  (p 'cdr))

(define (set-car! p x)
  ((p 'set-car!) x))
(define (set-cdr! p x)
  ((p 'set-cdr!) x))

(define (mlist . args)
  (if (null? args)
      null
      (mcons (car args)
             (apply mlist (cdr args)))))

(define (print-list ml)
  (cond [(procedure? ml)
         (display "(")
         (cond [(null? ml) (display "")]
               [(procedure? (mcar ml))
                (print-list (mcar ml))
                (display " . ")
                (print-list (mcdr ml))]
               [else
                (display (mcar ml))
                (display " . ")
                (print-list (mcdr ml))])
         (display ")")]
        [else (display ml)]))

;; Ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-cdr! x y)
          (loop temp x))))
    (loop x '()))
;; this procedure mutates the mlist into its reverse

;; Ex 3.17
(define count-pairs
  (let ((counted null))
    (lambda (x)
      (let ((check
             (lambda (p)
               (if (memq p counted)
                   0
                   (begin
                     (set! counted (cons p counted))
                     (count-pairs p))))))
        (if (not (pair? x))
            0
            (+ (check (car x))
               (check (cdr x))
               1))))))

(define (count-pairs* x)
  (define (inner x records)
    (if (and (pair? x)
             (not (memq x records)))
        (inner (car x)
               (inner (cdr x)
                      (cons x records)))
        records))
  (length (inner x null)))

;; Ex 3.18
(define (member? x ml)
  (cond [(null? ml) #f]
        [(eq? x (mcar ml)) #t]
        [else (member? x (mcdr ml))]))

(define (contain-cycle? x)
  (define (iter x record)
    (cond [(null? x) #f]
          [(not (procedure? x)) #f]
          [(member? (mcar x) record) #t]
          [else (iter (mcdr x) (mcons (mcar x) record))]))
  (iter x null))

;; one way is to set the car of the list into a unique identifier while traversing
;; the list so that when the program meet this identifier again, it knows the list
;; has a cycle, but this solution will destroy the original list
;; I try to find a way that will not mutate the original list
;; the following procedure walk through the list in two different speed: one with
;; one element per pace and the other with two elements per space. if they can meet,
;; we can conclude that the list has a cycle. actually I found this solution from
;; SICP 解题集
(define (contain-cycle?* x)
  (define (walk pace lst)
    (cond [(null? lst) null]
          [(= pace 0) lst]
          [else (walk (- pace 1)
                      (mcdr lst))]))
  (define (iter a b)
    (let ([walk-1 (walk 1 x)]
          [walk-2 (walk 2 x)])
      (cond [(or (null? walk-1)
                 (null? walk-2))
             #f]
            [(eq? walk-1 walk-2)
             #t]
            [else (iter walk-1 walk-2)])))
  (iter x x))
