#lang racket
(require "3.3.1_mutable-list.rkt")

(define (assoc key records)
  (cond [(null? records) #f]
        [(equal? key (caar records))
         (car records)]
        [else (assoc key (cdr records))]))

(define (lookup key table)
  (let ([record (assoc key table)])
    (if record
        (cdr record)
        #f)))

(define T
  (mlist '*table*
         (mcons 'a 1)
         (mcons 'b 2)
         (mcons 'c 3)
         (mcons 'd 4)))
