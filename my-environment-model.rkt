#lang racket
;(require "util.rkt")


;; I try to build a metacircular evaluator before I read Ch.4 here

;;utils
(define (last-item lst)
  (if (null? (cdr lst))
      (car lst)
      (last-item (cdr lst))))

;; data structures
(define (make-binding name value)
  (cons name value))

(define (make-environment binding-list parent)
  (let ((bindings (make-hash binding-list)))
    (lambda (request)
      (cond [(eq? request 'bindings) bindings]
            [(eq? request 'parent) parent]
            [(eq? request 'define)
             (lambda (name val)
               (dict-set! bindings name val))]
            [(eq? request 'set!)
             (lambda (name val)
               (if (dict-has-key? bindings name)
                   (dict-set! bindings name val)
                   (if (null? parent)
                       (error "unbound name: SET!" name)
                       ((parent 'set!) name val))))]
            [(eq? request 'lookup)
             (lambda (name)
                 (if (dict-has-key? bindings name)
                     (dict-ref bindings name)
                     (if (null? parent)
                         (error "unbound name:" name)
                         ((parent 'lookup) name))))]))))


;; global env
(define global
  (make-environment
   (list (make-binding '+ +)
         (make-binding '- -)
         (make-binding '* *)
         (make-binding '/ /)
         (make-binding 'remainder remainder)
         (make-binding 'abs abs)
         (make-binding 'expt expt)
         (make-binding 'sqrt sqrt)
         (make-binding 'log log)
         (make-binding 'cos cos)
         (make-binding 'sin sin)
         (make-binding 'tan tan)
         (make-binding '= =)
         (make-binding '> >)
         (make-binding '< <)
         (make-binding 'and (lambda (x y) (and x y)))
         (make-binding 'or (lambda (x y) (or x y)))
         (make-binding 'cons cons)
         (make-binding 'car car)
         (make-binding 'cdr cdr)
         (make-binding 'null null)
         (make-binding 'null? null?)
         (make-binding 'list list)
         (make-binding 'true #t)
         (make-binding 'false #f)
         (make-binding 'display display))
   null))

;; proc data struct
(define (make-proc lambda-exp env)
  (define (match-bindings names vals)
    (if (or (null? names) (null? vals))
        null
        (cons (make-binding (car names) (car vals))
              (match-bindings (cdr names) (cdr vals)))))
  (let ((apply
         (lambda (args)
           (letrec ((pars (cadr lambda-exp))
                    (body (cddr lambda-exp))
                    (local-env (make-environment (match-bindings pars args) env)))
             (if (= (length pars)
                    (length args))
                 (last-item (map (lambda (exp) (Eval exp local-env)) body))
                 (error "incorrect number of arguments:" lambda-exp args))))))
    (list 'compound apply)))
(define (compound-proc? proc)
  (and (list? proc) (eq? (car proc) 'compound)))
(define (apply-compound-proc proc args)
  ((cadr proc) args))

;; eval <-> apply
(define (Eval exp env)
  (define (operator exp)
    (car exp))
  (define (operands exp)
    (cdr exp))
  (cond [(or (number? exp) (string? exp)) exp]
        [(symbol? exp)
         ((env 'lookup) exp)]
        [(list? exp)
         ;(for-each display (list "eval: " exp)) (newline)
         (let ((op (operator exp)))
           (cond [(eq? 'define op)
                  (if (list? (cadr exp))
                      (letrec ((formal-exp (cadr exp))
                               (proc (car formal-exp))
                               (pars (cdr formal-exp))
                               (body (cddr exp)))
                        ((env 'define)
                         proc
                         (Eval (append (list 'lambda pars) body) env)))
                      (let ((name (cadr exp))
                            (body (caddr exp)))
                        (if (null? (cdddr exp))
                            ((env 'define) name (Eval body env))
                            (error "bad syntax: DEFINE" exp))))]
                 [(eq? op 'quote)
                  (cadr exp)]
                 [(eq? 'lambda op) (make-proc exp env)]
                 [(and (= (length exp) 3) (eq? 'set! op))
                  ((env 'set!) (cadr exp) (Eval (caddr exp) env))]
                 [(and (= (length exp) 4) (eq? 'if op))
                  (if (Eval (cadr exp) env)
                      (Eval (caddr exp) env)
                      (Eval (cadddr exp) env))]
                 [(eq? 'cond op)
                  (letrec ((cases (cdr exp))
                           (pred car)
                           (body cdr)
                           (check
                            (lambda (cases)
                              (cond
                                ((not (null? cases))
                                 (let ((case (car cases)))
                                   (if (or (eq? (pred case) 'else)
                                           (Eval (pred case) env))
                                       (last-item
                                        (map (lambda (e)
                                               (Eval e env))
                                             (body case)))
                                       (check (cdr cases)))))))))
                    (check cases))]
                 [else (Apply (Eval op env)
                              (map (lambda (e) (Eval e env))
                                   (operands exp)))]))]
        [else (error "bad expression: EVAL"
                     exp)]))


(define (Apply op args)
  (if (compound-proc? op)
      (apply-compound-proc op args)
      (apply op args))) ;apply primitive proc

(define (eval exp)
  (Eval exp global))


;; test
;; (eval '(define a 3))
;; (eval '(define b 4))
;; (eval '(define sq (lambda (x) (* x x)))) ;define functions
;; (eval '(define sq-sum (lambda (x y) (+ (sq x) (sq y))))) ;use previously defined functions
;; (eval '(define c (sqrt (sq-sum a b))))
;; (eval '(define a 5)) ;redefine variables
;; (eval '(= a c))

;; (eval '(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))) ;recursion is also available!
;; (eval '(factorial 6))

;; (eval '(define sum (lambda (term min max)
;;                     (if (> min max)
;;                         0
;;                         (+ (term min)
;;                            (sum term (+ min 1) max)))))) ;higher order procedure!
;; (eval '(define reciprocal-sum (lambda (min max) (sum (lambda (n) (/ 1 n)) min max))))
;; (eval '(reciprocal-sum 1.0 100.0))

;; (eval '(define (square x) (* x x)))
;; (eval '(define (average x y) (/ (+ x y) 2)))
;; (eval '(define (sqrt x)
;;         (define (good-enough? guess)  ;internal definition
;;           (< (abs (- (square guess) x)) 0.0001))
;;         (define (improve guess)
;;           (average guess (/ x guess)))
;;         (define (sqrt-iter guess)
;;           (if (good-enough? guess)
;;               guess
;;               (sqrt-iter (improve guess))))
;;         (sqrt-iter 1.0)))
;; (eval '(sqrt 4))

;; (eval '(define (make-dec n) (lambda (d) (set! n (- n d)) n)))  ;assignment
;; (eval '(define dec (make-dec 100)))
;; (eval '(dec 20))
;; (eval '(define dec1 (make-dec 100)))
;; (eval '(dec1 40))

;; (eval '(define a (list 1 2 3)))  ;list and list procedures
;; (eval '(define (map proc list)
;;         (if (null? list)
;;             null
;;             (cons (proc (car list))
;;                   (map proc (cdr list))))))
;; (eval '(define (append l1 l2)
;;          (if (null? l1)
;;              l2
;;              (cons (car l1)
;;                    (append (cdr l1) l2)))))
;; (eval '(map (lambda (x) (* 2 x)) a))
;; (eval '(define l '(a b 1 2)))
;; (eval '(append l l))

;; (eval '(define (filter pred list)
;;         (cond [(null? list) null]  ;cond
;;               [(pred (car list))
;;                (cons (car list)
;;                      (filter pred (cdr list)))]
;;               [else (filter pred (cdr list))])))
;; (eval '(filter (lambda (x) (= (remainder x 2) 0)) (list 2 3 4)))
