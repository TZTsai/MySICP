#lang racket
(require "sicp-lang.rkt" "3.3.3_Ex25_nD-table.rkt")

(define (memoize f)
  (let ([memo (make-table)])
    (let ([put (memo 'insert!)]
          [get (memo 'lookup)])
      (lambda (first . rest)
        (let ([args (cons first rest)])
          (let ([result (get args)])
            (if result
                result
                (let ([new-result
                       (apply f args)])
                  (for-each display
                            (list "records updated: "
                                  (memo 'show)))
                  (newline)
                  (put args new-result)
                  new-result))))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond [(= n 0) 0]
           [(= n 1) 1]
           [else
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2)))]))))

(define ackerman
  (memoize
   (lambda (m n)
     (cond [(= m 0) (+ n 1)]
           [(= n 0) (ackerman (- m 1) 1)]
           [else (ackerman (- m 1)
                           (ackerman
                            m
                            (- n 1)))]))))
