#lang racket
(require "sicp-lang.rkt" "util.rkt")
(provide stream-car stream-cdr stream-null? cons-stream
         the-empty-stream stream-ref stream-map stream-filter
         display-stream display-partial-stream force delay)


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (stream-filter pred s)
  (if (stream-null? s)
      the-empty-stream
      (if (pred (stream-car s))
          (cons-stream
           (stream-car s)
           (stream-filter pred (stream-cdr s)))
          (stream-filter pred (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each displayln s))

(define (display-partial-stream s k)
  (for-each (lambda (n)
              (displayln
               (stream-ref s n)))
            (enum 0 k)))

;; impl of stream
(define-syntax-rule (cons-stream a b)
  (cons a (delay b)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define the-empty-stream null)
(define stream-null? null?)



;; impl of delay and force
(define-syntax-rule (delay-simple exp)
  (lambda () exp))

;; use memoization to store the forced results
(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax-rule (delay-memo exp)
  (memo-proc (lambda () exp)))

(define-syntax-rule (delay exp)
  (delay-memo exp))

(define (force d)
  ;; (display "forcing ")
  ;; (displayln d)
  (d))



;; the efficiency of stream

;; give the second odd number in the
;; interval [10000, 1000000]:
;; note that the efficiency of this
;; solution in the common list interface
;; is extremely low

;; (stream-car 
;;  (stream-cdr
;;   (stream-filter 
;;    odd? (stream-enumerate-interval 
;;            10000 1000000))))



;; Ex 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

; samples
;; (define even-number-stream
;;   (stream-map (lambda (n) (* n 2))
;;               natural-number-stream))
;; (define triple-stream
;;   (stream-map +
;;               natural-number-stream
;;               even-number-stream))


;; Ex 3.51
;; (define (show x)
;;   (displayln x)
;;   x)

;; (define x 
;;   (stream-map 
;;    show 
;;    (stream-enumerate-interval 0 10)))

;; (stream-ref x 5)
;; (stream-ref x 7)

;; Ex 3.52

;; imp of delay:
;; (define-syntax-rule (delay exp)
;;   (lambda () exp))
;;   ;(memo-proc (lambda () exp)))

;; (define (show)
;;   (display "sum: ")
;;   (displayln sum))

;; (define sum 0)

;; (define (accum x)
;;   (set! sum (+ x sum))
;;   (show)
;;   sum)

;; (define seq 
;;   (stream-map 
;;    accum 
;;    (stream-enumerate-interval 1 20)))

;; (define y (stream-filter even? seq))

;; (define z 
;;   (stream-filter 
;;    (lambda (x) 
;;      (= (remainder x 5) 0)) seq))

;; (stream-ref y 7)

;; (display-stream z)
