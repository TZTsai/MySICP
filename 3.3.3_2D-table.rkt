#lang racket
(require "sicp-lang.rkt")
(provide (all-defined-out))

;; 2-D table
(define (lookup key1 key2 table)
  (let ([subtable (assoc key1 (cdr table))])
    (if subtable
        (let ([record (assoc key2 (cdr subtable))])
          (if record (cdr record) #f))
        #f)))

(define (insert! key1 key2 val table)
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

;; use message passing to create a local table
(define (make-table)
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

(define T (make-table))
(define get (T 'lookup))
(define put (T 'insert!))
