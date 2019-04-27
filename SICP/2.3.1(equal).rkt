#lang sicp

;ex 2.54
(define (equal? x y)
  (cond ((and (number? x) (number? y))
         (= x y))
        ((and (symbol? x) (symbol? y))
         (eq? x y))
        ((and (null? x) (null? y))
         #t)
        ((and (pair? x) (pair? y))
         (and (eq? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        (else #f)))

;ex 2.55
;''abracadabra is interpreted by the interpreter as (quote (quote abracadabra)), then
;evaluated as (list 'quote 'abracadabra). Hence, the car of it is 'quote