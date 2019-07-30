#lang racket
(require "sicp-lang.rkt" "3.3.2_queues.rkt" "util.rkt"
         (only-in (planet dyoo/sicp-concurrency:1:2/sicp-concurrency)
                  parallel-execute))

;; Ex 3.39
;; 121, 100, 101

;; Ex 3.40
;; 100, 1000, 10000, 100000, 1000000
;; After serialization, there is only the
;; possibility of 1000000 left

;; Ex 3.41
;; No. Checking balance is a single operation,
;; so it will not be interleaved by other ops.

;; Ex 3.44
;; He is wrong. The difference between exchange
;; and transfer is that in exchange, the access
;; of the difference between two accounts should
;; be exactly before the deposit and withdraw
;; procedures. Interleaving between them is not
;; allowed. However, the transfer has not this
;; limitation, as long as the withdraw will not
;; fail - the account has insufficient funds.

;; Ex 3.45
;; When we use serialized-exchange, serializer1
;; will be occupied, but when it calls exchange,
;; which will further call (account1 'withdraw),
;; if this process is also serialized, since the
;; serializer is occupied, it will never be allowed
;; to start, so serialized-exchange will never end.

;; my imp of serializer
(define (make-my-serializer)
  (let ([q (make-queue)]
        [started? #f])
    (define (run-first)
      (let ([first-proc (front-queue q)])
        (delete-queue! q)
        (first-proc)))
    (define (start!)
      (cond [(and (not (empty-queue? q))
                  (not started?))
             (set! started? #t)
             (run-first)]))
    (define (apply-and-start-next proc args)
      (lambda ()
        (apply proc args)
        (set! started? #f)
        (start!)))
    (lambda (proc)
      (define (run . args)
        (insert-queue!
         q
         (apply-and-start-next proc args))
        (start!))
      run)))

;; the imp in the book
(define (make-mutex)
  (define (clear! cell) (set-car! cell false))
  (define (test-and-set! cell)
    (if (car cell)
        true
        (begin (set-car! cell true)
               false)))
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;; serialized account
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
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) 
             (protected withdraw))
            ((eq? m 'deposit) 
             (protected deposit))
            ((eq? m 'balance) 
             balance)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;; Ex 3.47
;; util function
(define (enum n)
  (define (iter i l)
    (if (< i 0)
        l
        (iter (- i 1)
              (cons i l))))
  (iter (- n 1) null))
;; imp of semaphore
(define (make-semaphore n)
  (let ([mutexes (map (lambda (n) (make-mutex))
                      (enum n))])
    (let ([pointer mutexes])
      (let ([this-mutex (car pointer)])
        (define (switch-to-next)
          (set! pointer
                (if (null? pointer)
                    mutexes
                    (cdr pointer))))
        (lambda (p)
          (define (serialized-p . args)
            (this-mutex 'acquire)
            (switch-to-next)  ; here may lead to the piling up of tasks in a single mutex
            (let ((val (apply p args)))
              (this-mutex 'release)
              val))
          serialized-p)))))

(define (make-semaphore* n)
  (define (test-and-set!)  ; supposed to be atomic
    (if (= n 0)
        true
        (begin (set! n (- n 1))
               false)))
  (define (acquire)
    (if (test-and-set!)
        (acquire)))
  (define (dispatch m)
    (cond [(eq? m 'acquire)
           acquire]
          [(eq? m 'release)
           (set! n (+ n 1))]
          [else
           (error "unknown message" m)]))
  dispatch)

(define (make-serializer*)
  (let ((sema (make-semaphore* 3)))
    (lambda (p)
      (define (serialized-p . args)
        (sema 'acquire)
        (let ((val (apply p args)))
          (sema 'release)
          val))
      serialized-p)))

;; Ex 3.48
(define ID 1)
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer))
        (id ID))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'id) id)
            ((eq? m 'serializer) 
             balance-serializer)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    (set! ID (add1 ID))
    dispatch))

(define (get-serializer account)
  (account 'serializer))
(define (get-id account)
  (account 'id))

(define (serializer-avoid-deadlock . accounts)
  (define (sort accounts)
    (sorting-tool
     accounts
     (lambda (acc1 acc2)
       (number-compare
        (get-id acc1)
        (get-id acc2)))
     -1))
  (accumulate compose identity
              (map get-serializer
                   (sort accounts))))

(define (serialized-exchange* account1 account2)
  (let ((serializer (serializer-avoid-deadlock
                     account1 account2)))
    ((serializer exchange)
     account1
     account2)))

;; Ex 3.49
(define (make-address)
  (let ([address 1]
        [serializer (make-serializer)])
    (define (dispatch m)
      (cond [(eq? m 'get)
             address]
            [(eq? m 'set!)
             (lambda (n)
               (set! address n))]
            [(eq? m 'serializer)
             serializer]))
    dispatch))
(define A (make-address))
(define accounts
  (map (lambda (n) (make-account 100))
       (enum 10)))
(define (operation acc)
  (define (op)
    ((acc 'withdraw) 10)
    (let ([a (A 'get)])
      (((list-ref (- a 1))
        'deposit)
       10)))
  ((acc 'serializer)
   ((A 'serializer) op)))
;; Suppose we run (operation W),
;; since we cannot know in which account
;; we will deposit, we cannot serialize
;; it at the beginning. Suppose the account
;; going to be deposited is X, then if we
;; run (operation X) at the same time,
;; it cannot access A since (operation W)
;; has not exited. But (operation W) must
;; access X before it completes its procedure.
;; Hence a deadlock.

;; test
(define acc1 (make-account-and-serializer 100))
(define acc2 (make-account-and-serializer 200))
(define acc3 (make-account-and-serializer 300))
(parallel-execute
 (lambda () (serialized-exchange* acc1 acc2))
 (lambda () (serialized-exchange* acc2 acc3)))
(define (balance acc) (acc 'balance))
(map balance (list acc1 acc2 acc3))

;; test semaphore
;; (define s 10)
;; (define sema (make-serializer*))
;; (define (print-change p)
;;   (lambda ()
;;     (display s) (display "->")
;;     (p) (display s) (newline)))
;; (define (m n)
;;   (print-change
;;    (lambda ()
;;      (set! s (* s n)))))
;; (define (a n)
;;   (print-change
;;    (lambda ()
;;      (set! s (+ s n)))))
;; ;(parallel-execute
;; ; (m 2) (a 3) (m 3) (a -10))
;; (parallel-execute
;;  (sema (m 2)) (sema (a 3))
;;  (sema (m 3)) (sema (a -20)))
