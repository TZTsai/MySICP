#lang racket
(require "sicp-lang.rkt")
(provide (all-defined-out))

(define (make-table)
  (let ([table (list '*table*)])
    (define (lookup keys table)
      (let ([item (assoc (car keys) (cdr table))])
        (if item
            (if (null? (cdr keys))
                (cdr item)
                (lookup (cdr keys) item))
            #f)))
    (define (insert! keys val table)
      (define (new-entry keys val)
        (if (null? (cdr keys))
            (cons (car keys) val)
            (list (car keys)
                  (new-entry (cdr keys) val))))
      (let ([item (assoc (car keys) (cdr table))])
        (if item
            (if (null? (cdr keys))
                (set-cdr! item val)
                (insert! (cdr keys) val item))
            (set-cdr! table
                      (cons (new-entry keys val)
                            (cdr table))))))
    (define (dispatch m)
      (cond [(eq? m 'lookup)
             (lambda (keys) (lookup keys table))]
            [(eq? m 'insert!)
             (lambda (keys val) (insert! keys val table))]
            [(eq? m 'show)
             (cdr table)]
            [else (error "unknown operation: TABLE" m)]))
    dispatch))

(define T (make-table))
(define put (T 'insert!))
(define get (T 'lookup))

;; test
;; emulate the answers of an exam
(put '(A a i) 'B)
(put '(A b i) 72)
(put '(A b ii) 12)
(put '(B a) 'C)
(put '(A a ii) 4)
(put '(B b) 'A)
(put '(C a) 'false)
(put '(C b i) 12)
(put '(C b ii) 0)
(define T_A (get '(A)))
;; change an answer:
(put '(A a ii) 6)
;; the content of T_A is also changed

