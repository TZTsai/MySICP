#lang racket
(require "sicp-lang.rkt")
(provide make-table put get)

;; 1-D table
(define (assoc key records)
  (cond [(null? records) #f]
        [(equal? key (caar records))
         (car records)]
        [else (assoc key (cdr records))]))

(define (lookup key table)
  (let ([record (assoc key (cdr table))])
    (if record
        (cdr record)
        #f)))

(define (insert! key val table)
  (let ([record (assoc key (cdr table))])
    (if record
        (set-cdr! record val)
        (set-cdr! table
                  (cons (cons key val)
                        (cdr table)))))
  'ok)

(define (make-table*)
  (list '*table*))

;;message passing
(define (make-table)
  (let ([table (list '*table*)])
    (define (lookup key)
      (let ([record (assoc key (cdr table))])
        (if record
            (cdr record)
            #f)))
    (define (insert! key val)
      (let ([record (assoc key (cdr table))])
        (if record
            (set-cdr! record val)
            (set-cdr! table
                      (cons (cons key val)
                            (cdr table))))))
    (define (dispatch m)
      (cond [(eq? m 'lookup) lookup]
            [(eq? m 'insert!) insert!]
            [else (error "unknown operation: TABLE" m)]))
    dispatch))

(define T (make-table))
(define put (T 'insert!))
(define get (T 'lookup))