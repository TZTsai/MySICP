#lang racket
(require "sicp-lang.rkt")
(provide (all-defined-out))

;; vectors
(define (make-vect x y)
  (cons x y))
(define (vect-xcor v)
  (car v))
(define (vect-ycor v)
  (cdr v))

(define (vect-scale s v)
  (make-vect (* s (vect-xcor v))
             (* s (vect-ycor v))))
(define (vect-add v w)
  (make-vect (+ (vect-xcor v)
                (vect-xcor w))
             (+ (vect-ycor v)
                (vect-ycor w))))
(define (vect-sub v w)
  (vect-add v (vect-scale w -1)))

;; segments
(define (make-segment start end)
  (cons start end))
(define (segment-start seg)
  (car seg))
(define (segment-end seg)
  (cdr seg))

;; binary trees
(define (make-btree left entry right)
  (list left entry right))
(define (make-leaf entry)
  (make-btree nil entry nil))
(define (btree-left btree)
  (car btree))
(define (btree-entry btree)
  (cadr btree))
(define (btree-right btree)
  (caddr btree))

(define (empty? btree)
  (null? btree))
(define (leaf? btree)
  (and (not (empty? btree))
       (empty? (btree-left btree))
       (empty? (btree-right btree))))