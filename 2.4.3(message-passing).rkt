#lang sicp
(#%require "math.rkt" "data-structures.rkt")


(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x)
                    (square y))))
          ((eq? op 'angle)
           (atan y x))
          (else
           (error "Unknown op:
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* m (cos a)))
          ((eq? op 'imag-part)
           (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else
           (error "Unkown op: MAKE-FROM-MAG-ANG"
                  op))))
  dispatch)

(define (apply-generic op arg)
  (arg op))

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

;ex 2.76
#|
数据导向:数据导向可以很方便地通过包机制增加新类型和新的通用操作,因此无论是增加新类型还是增加新操作,这种策略都很适合。

消息传递：消息传递将数据对象和数据对象所需的操作整合在一起，因此它可以很方便地增加新类型，但是这种策略不适合增加新操作，
因为每次为某个数据对象增加新操作之后，这个数据对象已有的实例全部都要重新实例化才能使用新操作。
|#
