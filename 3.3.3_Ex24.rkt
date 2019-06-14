#lang racket
(require "sicp-lang.rkt")

(define (make-table same-key?)
  (define (assoc key records)
    (cond [(null? records) #f]
          [(same-key? key (caar records))
           (car records)]
          [else (assoc key (cdr records))]))
  (let ([table (list '*table*)])
    (define (lookup key1 key2)
      (let ([subtable (assoc key1 (cdr table))])
        (if subtable
            (let ([record (assoc key2 (cdr subtable))])
              (if record (cdr record) #f))
            #f)))
    (define (insert! key1 key2 val)
      (let ([subtable (assoc key1 (cdr table))])
        (if subtable
            (let ([record (assoc key2 (cdr subtable))])
              (if record
                  (set-cdr! record val)
                  (set-cdr! subtable (cons (cons key2 val)
                                           (cdr subtable)))))
            (set-cdr! table
                      (cons (list key1
                                  (cons key2 val))
                            (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond [(eq? m 'lookup) lookup]
            [(eq? m 'insert!) insert!]
            [else (error "unknown operation: TABLE" m)]))
    dispatch))

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))

(define T (make-table close-enough?))
(define put (T 'insert!))
(define get (T 'lookup))

(put 2 1 'hello)
(put 2 1.000002 'Hello)
(put 2 2 'bye)
