#lang sicp

;ex 2.17
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

;ex 2.18
(define (reverse l)
  (if (null? l)
      nil
      (append (reverse (cdr l))
              (list (car l)))))

;ex 2.19
(define (count-change amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coin-values)) 0)
        (else (+ (count-change amount
                               (cdr coin-values))
                 (count-change (- amount
                                  (car coin-values))
                               coin-values)))))
(define us-coins 
  (list 50 25 10 5 1))
(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

;ex 2.20
(define (same-parity first . rest)
  (define (same-parity-as-first? x)
    (= (remainder first 2)
       (remainder x 2)))
  (define (filtered rest)
    (if (null? rest)
        nil
        (let ((head (car rest))
              (tail (filtered (cdr rest))))
          (if (same-parity-as-first? head)
              (cons head tail)
              tail))))
  (cons first (filtered rest)))

;ex 2.23
(define (for-each proc lst)
  (cond ((not (null? lst))
         (proc (car lst))
         (for-each proc (cdr lst)))))

;ex 2.25
(define (cxxr str)
  (define (recur str lst)
    (let ((first (string-ref str 0))
          (rest (substring str 1)))
      (cond ((eq? first #\a)
             (car (recur rest lst)))
            ((eq? first #\d)
             (cdr (recur rest lst)))
            ((eq? first #\r)
             lst)
            (else (error "Unrecognizable symbol:" str)))))
  (lambda (lst)
    (if (eq? (string-ref str 0) #\c)
        (recur (substring str 1) lst)
        (error "Unrecognizable symbol:" str))))

(define (find a l)
  (define (searcher l)
    (cond ((null? l) #f)
          ((not (pair? l))
           (if (eq? a l)
               ""
               #f))
          (else (let ((a (searcher (car l)))
                      (d (searcher (cdr l))))
                  (cond (a (string-append a "a"))
                        (d (string-append d "d"))
                        (else #f))))))
  (let ((mid (searcher l)))
    (if mid
        (string-append "c" (searcher l) "r")
        "element not found")))
#| tests:
> (find 7 '(1 3 (5 7) 9))
"cadaddr"
> ((cxxr "cadaddr") '(1 3 (5 7) 9))
7
> (find 7 '(1 (2 (3 (4 (5 (6 7)))))))
"cadadadadadadr"
> ((cxxr "cadadadadadadr") '(1 (2 (3 (4 (5 (6 7)))))))
7
|#
;牛批啊！

;ex 2.27
(define (deep-reverse lst) 
  (if (pair? lst) 
      (append (deep-reverse (cdr lst))  
              (list (deep-reverse (car lst)))) 
      lst))

;ex 2.28
(define (fringe l)
  (cond ((null? l) nil)
        ((not (pair? l)) (list l))
        (else (append (fringe (car l))
                      (fringe (cdr l))))))

;ex 2.31
(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))
(define (map proc lst)
  (cond ((null? lst) nil)
        (else (cons (proc (car lst)) (map proc (cdr lst))))))
(define (tree-map* proc tree)
  (map (lambda (branch)
         (if (pair? branch)
             (tree-map* proc branch)
             (proc branch)))
       tree))

;ex 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append (map (lambda (subset)
                       (cons (car s) subset))
                     rest)
                rest))))
;这个sicp语言里面list的表示法也太鬼畜了吧。。。
;自己写一个。。。
(define (print-list l)
  (define (print-content l)
    (print-list (car l))
    (cond ((not (null? (cdr l)))
           (display " ")
           (print-content (cdr l)))))
  (cond ((null? l) (display "()"))
        ((not (pair? l)) (display l))
        (else (display "(")
              (print-content l)
              (display ")"))))
                     