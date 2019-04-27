#lang sicp

;;;sicp ex 2.16
(define (make-interval a b) 
  (if (< a b) 
      (cons a b) 
      (cons b a))) 
(define upper cdr) 
(define lower car)
(define lower-bound lower)
(define upper-bound upper)

(define (add-interval x y)
  (make-interval (+ (lower x) (lower y))
                 (+ (upper x) (upper y))))
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper y))
                               (- (lower y)))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (let ((y1 (upper y))
        (y2 (lower y)))
    (if (<= (* y1 y2) 0)
        (error "Division error (interval spans 0)" y)
        (mul-interval x 
                      (make-interval (/ 1.0 y1)
                                     (/ 1.0 y2))))))

;consider I = [0.5,2]
;I + 1/I should be [2, 2.5], rather than [1,4]
(define (calc-interval A B f constraint) ;给出一个x属于A，y属于B，x，y满足约束条件constraint的f(x,y)的值域
  "我想到了拉格朗日乘数法"
  )