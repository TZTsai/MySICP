#lang sicp

(define (average x y)
  (/ (+ x y) 2))

;ex 2.2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (midpoint segment)
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))

;ex 2.3
;First, by wishful thinking:
(define (perimeter-rect rect)
  (* 2 (+ (width-rect rect)
          (height-rect rect))))
(define (area-rect rect)
  (* (width-rect rect)
     (height-rect rect)))

;first implementation
(define (make-rect top-right bottom-left) ;this constructor accepts two points to make a rect
  (cons top-right bottom-left))
(define (tl-rect rect)
  (make-point (x-point (bl-rect rect))
              (y-point (tr-rect rect))))
(define (tr-rect rect)
  (car rect))
(define (bl-rect rect)
  (cdr rect))
(define (br-rect rect)
  (make-point (x-point (tr-rect rect))
              (y-point (bl-rect rect))))
(define (width-rect rect)
  (abs (- (x-point (tl-rect rect)) (x-point (tr-rect rect)))))
(define (height-rect rect)
  (abs (- (y-point (tl-rect rect)) (y-point (bl-rect rect)))))

;second implementation
(define (make-rect* midpoint width height) ;this constructor accepts one point and two numbers
  (cons midpoint (cons width height)))
(define (width-rect* rect)
  (car (cdr rect)))
(define (height-rect* rect)
  (cdr (cdr rect)))