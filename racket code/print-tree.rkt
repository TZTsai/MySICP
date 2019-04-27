#lang racket
(require "util.rkt" "data-structures.rkt")
(require (except-in "sicp-lang.rkt"
                    letrec))

(define (half1 n)
  (quotient (+ n 1) 2))
(define (half2 n)
  (quotient n 2))

;;; I assume that the 

(define (make-comb w p h d)
  (cons (list w p h) d))
(define width caar)
(define pos cadar)
(define height caddar)
(define disp cdr)

(define (string->comb str)
  (let ([len (string-length str)])
    (make-comb len (half1 len) 1
               (lambda (k)
                 (if (= k 1)
                     (display str)
                     (display (blank-str len)))))))
(define (exp->comb exp)
  (string->comb (->string exp)))

(define (blank-str len)
  (make-string len #\ ))

(define (make-blank w h)
  (make-comb w (half1 w) h
             (lambda (k)
               (display
                (blank-str w)))))

(define (make-joint top bl br)
  (letrec ([mid (pos top)]
           [left (if (null? bl) mid (pos bl))]
           [right (if (null? br) mid (+ (if (null? bl) 0 (width bl)) (pos br)))]
           [give-char
            (lambda (k)
              (cond ((or (< k left) (> k right)) #\ )
                    ((= k mid) #\^)
                    ((or (= k left) (= k right)) #\+)
                    (else #\-)))])
    (string->comb (build-string (width top) give-char))))

(define (enlarge comb lblk rblk)
  (if (null? comb)
      (make-blank (+ lblk rblk) 1)
      (letrec ([hblk (height comb)])
        (beside (make-blank lblk hblk)
                comb
                (make-blank rblk hblk)))))

(define (fit comb w)
  (letrec ([dw (- w (width comb))]
           [lb (half1 dw)]
           [rb (half2 dw)])
    (enlarge comb lb rb)))
  
(define (beside . combs)
  (let ([d (lambda (n)
             (for-each
              (lambda (c)
                ((disp c) n))
              combs))]
        [w (accumulate + 0 (map width combs))]
        [p (+ (width (car combs)) (pos (cadr combs)))] ;let the pos be the pos of the second comb (specifically for enlarge)
        [h (list-max (map height combs))])
    (make-comb w p h d)))

(define (above . combs)
  (define (find-disp n combs)
    (let ((h1 (height (car combs))))
      (if (or (null? (cdr combs))
              (<= n h1))
          ((disp (car combs)) n)
          (find-disp (- n h1) (cdr combs)))))
  (let ([heights (map height combs)])
    (let ([d (lambda (n) (find-disp n combs))]
          [w (width (car combs))]
          [p (pos (car combs))]
          [h (sum heights)])
      (make-comb w p h d))))

(define (print-comb comb)
  (for-each (lambda (n)
              ((disp comb) n)
              (newline))
            (enum 1 (height comb))))

(define (make-Y top bl br)
  (if (and (null? bl) (null? br))
      (enlarge top 1 1)
      (letrec ([ensure-comb (lambda (comb)
                              (enlarge comb 0 0))]
               [left (ensure-comb bl)]
               [right (ensure-comb br)]
               [wt (width top)]
               [wbl (if (null? left) 0 (width left))]
               [wbr (if (null? right) 0 (width right))]
               [w (+ wbl wbr)]
               [dw (- wt w)])
        (cond ((< dw 0)
               (let ([enlarged-entry
                      (fit top w)])
                 (above enlarged-entry
                        (make-joint
                         enlarged-entry
                         left right)
                        (beside left right))))
              ((= dw 0)
               (above top
                      (make-joint
                       top left right)
                      (beside left right)))
              ((> dw 0)
               (let ([enlarged-left
                      (enlarge left 0 (half1 dw))]
                     [enlarged-right
                      (enlarge right (half2 dw) 0)])
                 (above top
                        (make-joint
                         top enlarged-left
                         enlarged-right)
                        (beside
                         enlarged-left
                         enlarged-right))))))))

                
(define (btree->comb tree)
  (if (null? tree)
      null
      (make-Y
       (exp->comb (btree-entry tree))
       (btree->comb (btree-left tree))
       (btree->comb (btree-right tree)))))

(define (print-btree tree)
  (print-comb (btree->comb tree)))



;;; for test
(define c1 (exp->comb '[1 2 3]))
(define c2 (exp->comb '[4 5]))
(define y (make-Y c1 null c2))
(define t (ordered-set->btree (enum 1 40)))
(print-btree t)

#| test result:
> (print-btree t)
                             20                             
              +---------------^-------------+               
             10                            30               
       +------^-----+               +-------^-------+       
      5            15              25              35       
    +--^--+     +---^---+       +---^---+       +---^---+   
   2     7     12      17      22      27      32      38   
  +-^+  +-^+  +-^-+   +-^-+   +-^-+   +-^-+   +-^-+   +-^-+ 
 1  3  6  8  11  13  16  18  21  23  26  28  31  33  36  39 
   +-^   +-^    +-^     +-^     +-^     +-^     +-^ +-^ +-^ 
    4     9      14      19      24      29      34  37  40 

Not perfect.
|#