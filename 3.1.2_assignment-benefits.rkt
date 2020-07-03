#lang racket
(require "math.rkt")


(define rand-init 723311)
(define rand-max 100000000)

(define (rand-update x)
  (define (generator a b m)
    (lambda (x)
      (remainder (+ (* a x) b) m)))
  ((generator 19721 243217 rand-max) x))


;; (define rand
;;   (let ((x rand-init))
;;     (lambda () (set! x (rand-update x)) x)))
;; 这个定义被Ex 3.6的新定义替代了


;; Monte Carlo method
(define (estimate-pi trial-number)
  (sqrt (/ 6 (monto-carlo trial-number cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand 'generate) (rand 'generate)) 1))

(define (monto-carlo trial-number experiment)
  (define (iter passed-trials rest-trials)
    (cond [(= rest-trials 0)
           (/ passed-trials trial-number)]
          [(experiment)
           (iter (add1 passed-trials)
                 (sub1 rest-trials))]
          [else
           (iter passed-trials
                 (sub1 rest-trials))]))
  (iter 0 trial-number))

;; not using assignment, so use rand-update instead of rand
(define (estimate-pi* trials)
  (sqrt (/ 6 (monto-carlo* trials rand-init))))

(define (monto-carlo* trials init)
  (define (iter passed rest x)
    (letrec ([x1 (rand-update x)]
             [x2 (rand-update x1)])
      (cond [(= rest 0) (/ passed trials)]
            [(= (gcd x1 x2) 1) (iter (add1 passed)
                                     (sub1 rest)
                                     x2)]
            [else (iter passed
                        (sub1 rest)
                        x2)])))
  (iter 0 trials init))
;; the disadvantage of this program is that MONTO-CARLO* is not a
;; general method which can take an arditrary EXPERIMENT procedure
;; like MONTO-CARLO. Thus it does not act as an independent module
;; which can be used in other tasks requiring the monto-carlo method.
;; Actually, better not to name this program MONTO-CARLO*, for it can
;; only be intertwined with the (= (gcd x1 x2) 1) test.

;; Ex 3.5
(define (rand-in-range min max)
  (+ min (* (/ (rand 'generate) rand-max)
            (- max min))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ([experiment (λ () (P (rand-in-range x1 x2)
                              (rand-in-range y1 y2)))])
    (monto-carlo trials experiment)))

(define (estimate-pi** trials)
  (* 4.0 (estimate-integral (λ (x y) (<= (sq-sum x y) 1))
                          -1 1 -1 1 trials)))

;; Ex 3.6
(define rand
  (let ([x rand-init])
    (lambda (command)
      (cond [(eq? command 'generate)
             (set! x (rand-update x)) x]
            [(eq? command 'reset)
             (lambda (new-val) (set! x new-val) x)]
            [else
             (error "Unknown command: RAND" command)]))))
;; 一个问题：为什么我直接写(define (rand command) (let ([x rand-init]) ...)的时候，
;; (rand 'generate)得到的结果一直是相同的，而我把(lambda (command) ...)写到
;; (let ([x rand-init]))里面之后就可以得到不同的随机数了？
;; 我的解答：这就是lexical scope的关系. 如果let在 lambda的作用域内部, 那么每次调用
;; rand时, x的值都是相同的; 然而如果在lambda的作用域之外, x就是独立于每次对rand的
;; 调用了.
