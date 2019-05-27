#lang racket
(require "sicp-lang.rkt" "util.rkt")

(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch tag)
    (cond [(eq? tag 'withdraw) withdraw]
          [(eq? tag 'deposit) deposit]
          [else (error "Unknown request: MAKE-ACCOUNT"
                       tag)]))
  dispatch)


;; Ex 3.1
(define (make-accumulator sum)
  (λ (n) (set! sum (+ sum n)) sum))

;; Ex 3.2
(define (make-monitored f)
  (define calls-count 0)
  (define (reset!) (set! calls-count 0))
  (define (apply-call args)
    (set! calls-count (inc calls-count))
    (apply f args))
  (define (dispatch . args)
    (cond [(eq? (car args) 'how-many-calls?) calls-count]
          [(eq? (car args) 'reset-count) (reset!)]
          [else (apply-call args)]))
  dispatch)

; for test
(define mdec (make-monitored dec))
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (mdec n)))))

;; Ex 3.3, 3.4
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
  (define (dispatch pswd tag)
    (cond [frozen?
           (lambda (amount)
             (display "Sorry, your account is frozen. Please contact the bank clerk."))]
          [(not (eq? pswd my-pswd))
           (λ (amount)
             (set! failed-times (inc failed-times))
             (display "Incorrect password")
             (cond [(= failed-times 3)
                    (set! frozen? #t)
                    (newline)
                    (display "Sorry, your account is frozen. Please contact the bank clerk.")]))]
          [else (begin (set! failed-times 0)
                       (cond [(eq? tag 'withdraw) withdraw]
                             [(eq? tag 'deposit) deposit]
                             [else (error "Unknown request: MAKE-ACCOUNT"
                                          tag)]))]))
  dispatch)
(define my-acc (make-protected-account 1000 666))