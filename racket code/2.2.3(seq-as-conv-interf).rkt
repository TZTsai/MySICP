#lang sicp

(define (filter predicate sequence)
  (if (null? sequence)
      nil
      (let ((rest (filter predicate 
                          (cdr sequence))))
        (if (predicate (car sequence))
            (cons (car sequence) rest)
            rest))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

;ex 2.33
(define (map* p seq)
  (accumulate (lambda (x y) (cons (p x) y))
              nil seq))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length seq)
  (accumulate (lambda (x y) (inc y)) 0 seq))

;ex 2.34
(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

;ex 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree t))))

;ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;ex 2.37
(define (transpose m)
  (accumulate-n cons nil m))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
;the map above makes use of the more general version of map, see sicp footnote 78

(define (mat-vec-product m v)
  (map (lambda (row) (dot-product row v))
       m))
(define (mat-mat-product m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (mat-vec-product cols row))
         m)))
;I can extend this exercise further to play around other concepts in linear algebra
;for instance, determinant, dimension, independence, linear combination, matrix inverse, etc.

;ex 2.38
(define fold-right accumulate)
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter init seq))

;ex 2.39
(define (reverse-fr seq)
  (fold-right
   (lambda (x y) (append y (list x)))
   nil seq))
(define (reverse-fl seq)
  (fold-left
   (lambda (x y) (cons x y)) nil seq))

;for test
(define t (list 1 (list 2 (list 3 4)) 5))
(define mat1 (list (list 1 2 3) (list 40 50 60) (list 700 800 900)))
(define mat2 (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
(define (print-list l)
  (define (print-content l)
    (print-list (car l))
    (cond ((not (null? (cdr l)))
           (display " ")
           (print-content (cdr l)))))
  (cond ((null? l) (display "()"))
        ((not (pair? l)) (display l))
        (else (display "(")
              (print-content l)
              (display ")"))))



;next section: Nested Mappings
(define (flat-map proc seq)
  (accumulate append
              nil
              (map proc seq)))

(define (permutations set)
  (define (remove x set)
    (filter (lambda (el) (not (eq? el x))) set))
  (if (null? set)
      (list nil)
      (flat-map (lambda (el)
                  (map (lambda (perm)
                         (cons el perm))
                       (permutations (remove el set))))
                set)))

;ex 2.40
(define (unique-pairs n)
  (flat-map (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

;ex 2.41
(define (unique-triples n)
  (flat-map (lambda (i)
              (map (lambda (p) (cons i p))
                   (unique-pairs (- i 1))))
            (enumerate-interval 1 n)))
(define (sum seq)
  (accumulate + 0 seq))
(define (triples-with-sum n s)
  (filter (lambda (tri) (= (sum tri) s))
          (unique-triples n)))

;ex 2.42
(define (queens board-size)
  (define empty-config nil)
  (define (safe? new-pos current-config)
    (define (check? pos1 pos2 cols-apart)
      (or (= pos1 pos2)
          (= pos1 (- pos2 cols-apart))
          (= pos1 (+ pos2 cols-apart))))
    (define (recur config distance)
      (if (null? config)
          #t
          (and (not (check? new-pos (car config) distance))
               (recur (cdr config) (inc distance)))))
    (recur current-config 1))
  (define (partial k)
    (if (zero? k)
        (list empty-config)
        (flat-map (lambda (current-config)
                    (map (lambda (safe-pos)
                           (cons safe-pos current-config))
                         (filter (lambda (new-pos)
                                   (safe? new-pos current-config))
                                 (enumerate-interval 1 board-size))))
                  (partial (dec k)))))
  (partial board-size))

;I want to find out only the fundemental solutions, i.e., solutions that cannot be transformed
;into each other by rotation or reflection

(define (member? equiv? x lst)
  ;use EQUIV? to determine whether X is equivalent to some element in LST
  (accumulate (lambda (x y) (or x y))
              #f
              (map (lambda (y) (equiv? x y))
                   lst)))

(define (select-distinct equiv? lst)
  ;select all the distinct elements in the LST, using EQUIV? to judge whether two elements are equivalent
  (define (iter set lst)
    (cond ((null? lst) set)
          ((member? equiv? (car lst) set)
           (iter set (cdr lst)))
          (else (iter (cons (car lst) set)
                      (cdr lst)))))
  (iter nil lst))

(define (queens-sol-eq? sol1 sol2)
  ;determine whether two solutions SOL1 and SOL2 of queens problem are equivalent
  (define board-size (length sol1))
  (define (reflect sol)
    (reverse-fl sol))
  (define (find-col row sol)
    ;find which column the queen is in
    (define (iter k cols)
      (if (eq? row (car cols))
          k
          (iter (inc k) (cdr cols))))
    (iter 1 sol))
  (define (rot90 sol) ;rotate by 90 degrees
    (map (lambda (pos)
           (- (+ board-size 1) (find-col pos sol)))
         (enumerate-interval 1 board-size)))
  (define (rot180 sol)
    (rot90 (rot90 sol)))
  (define (rot270 sol)
    (rot90 (rot180 sol)))
  (define (symmetry-group sol)
    (let ((ref (reflect sol)))
      (list sol (rot90 sol) (rot180 sol) (rot270 sol)
            ref (rot90 ref) (rot180 ref) (rot270 ref))))
  (member? equal? sol1 (symmetry-group sol2)))

(define (fundamental-queens board-size)
  (select-distinct queens-sol-eq? (queens board-size)))
#|tests:
> (length (fundamental-queens 8))
12
> (length (queens 8))
92
> (length (fundamental-queens 6))
1
> (length (queens 6))
4
These give the same results as in Wikipedia.
|#