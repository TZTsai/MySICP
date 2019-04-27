#lang racket
(require sicp-pict)

;;;utility
(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeated f n)
  (define (iter g cnt)
    (if (= cnt n)
        g
        (iter (compose f g) (add1 cnt))))
  (iter identity 0))

#|
;;;vector
(define (make-vect x y)
  (cons x y))
(define (vector-xcor v)
  (car v))
(define (vector-ycor v)
  (cdr v))

(define (vector-scale s v)
  (make-vect (* s (vector-xcor v))
               (* s (vector-ycor v))))
(define (vector-add v w)
  (make-vect (+ (vector-xcor v)
                  (vector-xcor w))
               (+ (vector-ycor v)
                  (vector-ycor w))))
(define (vector-sub v w)
  (vector-add v (vector-scale w -1)))

;;;frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (frame-origin frame)
  (car frame))
(define (frame-edge1 frame)
  (cadr frame))
(define (frame-edge2 frame)
  (caddr frame))

;;;segment
(define (make-segment start end)
  (cons start end))
(define (segment-start seg)
  (car seg))
(define (segment-end seg)
  (cdr seg))
|#

;;;transform a painter by means of transforming the frame
(define (frame-coord-map frame)
  (lambda (v)
    (vector-add (frame-origin frame)
                (vector-add (vector-scale (vector-xcor v)
                                          (frame-edge1 frame))
                            (vector-scale (vector-ycor v)
                                          (frame-edge2 frame))))))

(define (transform-painter
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (vector-sub (m corner1)
                                         new-origin)
                             (vector-sub (m corner2)
                                         new-origin)))))))

;;;define primitive operations with respect to transform-painter
(define (flip-vert painter)
  (transform-painter
   painter
   (make-vect 0.0 1.0)
   (make-vect 1.0 1.0)
   (make-vect 0.0 0.0)))
(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define rotate180 (repeated rotate90 2))
(define rotate270 (repeated rotate90 3))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
(define (scale painter x-scale y-scale) ;x-scale, y-scale should be no more than 1
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (vector-scale x-scale (make-vect 1 0))
                     (vector-scale y-scale (make-vect 0 1))))
(define (translate painter x-distance y-distance)
  (let ((dv (make-vect x-distance y-distance)))
    (transform-painter painter
                       dv
                       (vector-add dv (make-vect 1 0))
                       (vector-add dv (make-vect 0 1)))))
(define (beside painter1 painter2)
  (let ((left (scale painter1 0.5 1))
        (right (translate (scale painter2 0.5 1) 0.5 0)))
    (lambda (frame)
      (left frame)
      (right frame))))
(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter2)
                     (rotate90 painter1))))

;ex 2.49
(define square
  (let ((bl (make-vect 0 0))
        (tl (make-vect 0 1))
        (br (make-vect 1 0))
        (tr (make-vect 1 1)))
    (let ((l (make-segment bl tl))
          (t (make-segment tl tr))
          (r (make-segment br tr))
          (b (make-segment bl br)))
      (segments->painter (list l t r b)))))

(define x
  (let ((bl (make-vect 0 0))
        (tl (make-vect 0 1))
        (br (make-vect 1 0))
        (tr (make-vect 1 1)))
    (let ((diag1 (make-segment bl tr))
          (diag2 (make-segment br tl)))
      (segments->painter (list diag1 diag2)))))

(define diamond
  (let ((t (make-vect 0.5 1))
        (b (make-vect 0.5 0))
        (l (make-vect 0 0.5))
        (r (make-vect 1 0.5)))
    (let ((tl (make-segment t l))
          (tr (make-segment t r))
          (bl (make-segment b l))
          (br (make-segment b r)))
      (segments->painter (list tl tr bl br)))))

;ex 2.45
(define (split comb-big-small comb-small-small)
  (define (spliter painter n)
    (if (zero? n)
        painter
        (let ((smaller (spliter painter (- n 1))))
          (comb-big-small painter
                          (comb-small-small smaller smaller)))))
  spliter)

;;;higher order operations on painters
(define up-split (split below beside))
(define right-split (split beside below))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter n))
            (right (right-split painter n)))
        (beside (below painter (beside up up))
                (below (below right right) (corner-split painter (- n 1)))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define (whirl painter)
  (let ((combine4 
         (square-of-four rotate90
                         identity
                         rotate180 
                         rotate270)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip-horiz 
                         identity
                         rotate180 
                         flip-vert)))
    (combine4 (corner-split painter n))))

;;;my own higher-order operations

;tree recursion
(define (tree painter n)
  (if (<= n 1)
      painter
      (below painter
             (beside (tree painter (- n 1))
                     (tree painter (- n 2))))))

;some extended beside and below's
(define (weighted-beside p1 p2 w1) ;W1 the width of P1
  (let ((left (scale p1 w1 1))
        (right (translate (scale p2 (- 1 w1) 1) w1 0)))
    (lambda (frame)
      (left frame)
      (right frame))))
(define (weighted-below p1 p2 w1)
  (rotate270 (weighted-beside (rotate90 p2)
                              (rotate90 p1) (- 1 w1))))
(define (beside3 p1 p2 p3)
  (weighted-beside p1 (beside p2 p3) (/ 1 3)))

(define (tornado painter n)
  (if (= n 0)
      painter
      (weighted-below (weighted-beside (below (rotate90 painter) (rotate90 painter))
                                       (rotate180 (tornado painter (sub1 n)))
                                       (/ 1 3))
                      (beside3 painter painter painter)
                      (/ 2 3))))