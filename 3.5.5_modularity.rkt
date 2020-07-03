#lang racket
(require
 "util.rkt"
 (except-in "math.rkt" prime?)
 "3.5.1_stream.rkt"
 "3.5.2_infinite-stream.rkt")


;; impl a rand generator using stream
(define rand-init 72311)
(define rand-max 100000000)

(define (rand-update x)
  (define (generator a b m)
    (lambda (x)
      (remainder (+ (* a x) b) m)))
  ((generator 19721 243217 rand-max) x))

(define rand-stream
  (cons-stream rand-init
               (stream-map rand-update rand-stream)))


;; Cesaro experiment
(define (map-successive-pairs f s)
  (cons-stream (f (stream-car s)
                  (stream-car (stream-cdr s)))
               (map-successive-pairs
                f (stream-cdr s))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        rand-stream))


;; Monto-carlo proc (also a stream, producing
;; better and better estimates)
(define (monto-carlo experiment-stream)
  (define (iter trials passed stream)
    (let ([updated-passed
           (if (stream-car stream)
               (+ passed 1) passed)]
          [updated-trials (+ trials 1)])
      (cons-stream (/ updated-passed
                      updated-trials)
                   (iter updated-trials
                         updated-passed
                         (stream-cdr stream)))))
  (iter 0 0 experiment-stream))

(define pi-stream (stream-map (lambda (p) (sqrt (/ 6 p)))
                       (monto-carlo cesaro-stream)))

; (stream-ref pi-stream 1000)


;; Ex 3.81
(define (rand-stream-as-requested request-stream)
  (define rand-stream
    (cons-stream rand-init
                 (stream-map
                  (lambda (num req)
                    (cond [(eq? req 'generate)
                           (rand-update num)]
                          [(and (pair? req)
                                (eq? (car req) 'reset))
                           (cadr req)]))
                  rand-stream
                  request-stream)))
  rand-stream)

; test
(define rs (cons-stream
            'generate
            (cons-stream
             'generate
             (cons-stream
              'generate
              (cons-stream
               '(reset 10) rs)))))

; (display-partial-stream (rand-stream-as-requested rs) 15)


;; Ex 3.82
(define (rand-stream-in-range min max)
  (stream-map (lambda (n)
                (+ min (* (/ n rand-max)
                          (- max min))))
              rand-stream))

(define (estimate-integral P x1 x2 y1 y2)
  (let ([experiment
         (stream-map P (rand-stream-in-range x1 x2)
                     (rand-stream-in-range y1 y2))])
    (monto-carlo experiment)))

(define pi-stream*
  (scale-stream (estimate-integral (λ (x y) (<= (sq-sum x y) 1))
                                   -1 1 -1 1)
                4.0))

(stream-ref pi-stream* 1000)
