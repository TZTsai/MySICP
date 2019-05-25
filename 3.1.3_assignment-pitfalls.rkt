#lang racket


(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))


(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))


;; evaluation:
;; (W 20)
;; 5
;; (W 10)
;; -5
;; (D 20)
;; 5
;; (D 10)
;; 15


;; the substitution model can well explain the applications of D on 20 and 10:
;; (D 20) -> ((lambda (amount) (- 25 amount)) 20) -> (- 25 20) -> 5,
;; while it cannot account for the evaluations of (W 20) & (W 10).
;; Let's try to evaluate (W 20):
;; (W 20) -> ((lambda (amount) (set! balance (- 25 amount) 25) 20)
;;        -> (set! balance (- 25 20)) 25 -> 25, which is a wrong answer
;; The key point is that in the substitution model, a variable
;; is just a name for a certain value, while in the evaluation of (W 20) BALANCE
;; before set! and after set! are distinct, but the substitution model cannot
;; tell the difference. After introducing SET!, a
;; name becomes somehow a box where a value is stored - though the stored value
;; changes through time, the container maintains. This is the idea of
;; imperative programming.


;; defining factorial in a more imperative way
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter
                                  product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))


;; Ex 3.7
(define (make-protected-account balance my-pswd)
  (define failed-times 0)
  (define frozen? #f)
  (define (withdraw amount)
    (if (>= balance amount)
          (begin (set! balance
                       (- balance amount))
                 balance)
          (display "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pswd request)
    (cond [frozen?
           (lambda (amount)
             (display "Sorry, your account is frozen. Please contact the bank clerk."))]
          [(not (eq? pswd my-pswd))
           (λ (amount)
             (set! failed-times (add1 failed-times))
             (display "Incorrect password")
             (cond [(= failed-times 3)
                    (set! frozen? #t)
                    (newline)
                    (display "Sorry, your account is frozen. Please contact the bank clerk.")]))]
          [else (set! failed-times 0)
                (cond [(eq? request 'withdraw) withdraw]
                      [(eq? request 'deposit) deposit]
                      [else (error "Unknown request: MAKE-ACCOUNT"
                                   request)])]))
  dispatch)

(define (make-joint account orig-pswd new-pswd)
  (lambda (pswd request)
    (if (eq? pswd new-pswd)
        (account orig-pswd request)
        (let ([wrong-pswd (if (eq? 1 orig-pswd) 0 1)])
          (account wrong-pswd request)))))

(define acc1 (make-protected-account 100 1234))
(define acc2 (make-joint acc1 1234 4321))


;; Ex 3.8
(define f
  (let ([n 1])
    (lambda (m)
      (if (= m 0)
          (begin (set! n 0) n)
          n))))

(define (test-eval-order)
  (let ([n (+ (f 0) (f 1))])
    (if (= n 0)
        'left->right
        'right->left)))
