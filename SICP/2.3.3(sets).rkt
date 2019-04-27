#lang sicp
(#%require "util.rkt")


;;; sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) nil)
        ((element-of-set? (car s1) s2)
         (cons (car s1)
               (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else (cons (car s1)
                    (union-set (cdr s1) s2)))))

;ex 2.60
(define (element-of-set?* x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set* x set)
  (cons x set))

(define (intersection-set* s1 s2)
  ;This inner definition of "select" is designed for time efficiency.
  ;Because to find the intersection of two lists (which means duplicate items may exist) (I don't like calling
  ;them as "sets" as done in the book because a set is defined to have distinct elements), for each item in list1,
  ;whenever you find an equal item in list2, you should delete it in case another item in list1 might be matched
  ;to it again. The searching for an equal item to the item in list1 has linear time cost, and if I want to use the
  ;primitive procedure "remove", it will traverse list2 again to find the item which I already found! So a more
  ;intricate version of "remove" (I call it "select") is needed. Order in the list is overlooked.
  (define (select x set) 
    (define (iter done left)
      (cond ((null? left) (cons nil done))
            ((eq? x (car left)) (cons (list x) (append done (cdr left))))
            (else (iter (cons (car left) done) (cdr left)))))
    (iter nil set))
  (if (null? s1)
      nil
      (let ((select-result (select (car s1) s2)))
        (let ((selected (car select-result))
              (left (cdr select-result)))
          (append selected
                  (intersection-set* (cdr s1) left))))))

(define (union-set* s1 s2)
  (append s1 s2))


;;; sets as ordered lists
(define (bigger? x y)
  ;(display x) (display " compare key ") (display y) (newline)
  (let ((ascii
         '[0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f g h i j k l m n o p q r s t u v w x y z]))
    (let ((x-pos (member x ascii))
          (y-pos (member y ascii)))
      (if (and x-pos y-pos)
          (< (length x-pos)
             (length y-pos))
          (error "at least one symbol not found: BIGGER?"
                 x y)))))
(define (smaller? x y)
  (not (or (bigger? x y) (eq? x y))))

(define (ascii-compare a b)
  (cond ((bigger? a b) 1)
        ((smaller? a b) -1)
        (else 0)))

(define (sort set compare) ;in increasing order
  (define (divide x set)
    (define (iter small big set)
      ;(display x) (display " record vs db ") (display set) (newline)
      (if (null? set)
          (cons small big)
          (if (> (compare x (car set)) 0)
              (iter (cons (car set) small) big (cdr set))
              (iter small (cons (car set) big) (cdr set)))))
    (iter nil nil set))
  (if (<= (length set) 1)
      set
      (let ((divided (divide (car set) (cdr set))))
        (append (sort (car divided) compare)
                (list (car set))
                (sort (cdr divided) compare)))))

(define (element-of-ordered-set? x set)
  (cond ((null? set) false)
        ((bigger? (car set) x) false)
        ((eq? x (car set)) true)
        (else (element-of-ordered-set? x (cdr set)))))

;ex 2.61
(define (adjoin-ordered-set x set)
  (cond ((null? set) (list x))
        ((bigger? (car set) x) (cons x set))
        ((eq? x (car set)) set)
        (else (cons (car set)
                    (adjoin-ordered-set x (cdr set))))))

(define (inter-ordered-set s1 s2)
  (cond ((or (null? s1) (null? s2)) nil)
        ((bigger? (car s1) (car s2))
         (inter-ordered-set s1 (cdr s2)))
        ((eq? (car s1) (car s2))
         (cons (car s1) (inter-ordered-set (cdr s1) (cdr s2))))
        (else
         (inter-ordered-set (cdr s1) s2))))

;ex 2.62
(define (union-ordered-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1)) (x2 (car s2)))
           (cond ((bigger? x1 x2)
                  (cons x2 (union-ordered-set s1 (cdr s2))))
                 ((eq? x1 x2)
                  (cons x1 (union-ordered-set (cdr s1) (cdr s2))))
                 (else
                  (cons x1 (union-ordered-set (cdr s1) s2))))))))


;;; sets as binary trees
(define (make-tree left entry right)
  (list left entry right))
(define (make-leaf entry)
  (make-tree nil entry nil))
(define (tree-left tree)
  (car tree))
(define (tree-entry tree)
  (cadr tree))
(define (tree-right tree)
  (caddr tree))

(define (empty? tree)
  (null? tree))
(define (leaf? tree)
  (and (not (empty? tree))
       (empty? (tree-left tree))
       (empty? (tree-right tree))))

(define (count-leaves tree)
  (if (empty? tree)
      0
      (+ 1
         (count-leaves (tree-left tree))
         (count-leaves (tree-right tree)))))

;my own way of printing a btree
(define (print-tree tree)
  (define (print-branch br dir)
    (if (not (empty? br))
        (if (eq? dir '<)
            (begin (print-tree br) (display "<-"))
            (begin (display "->") (print-tree br)))))
  (if (not (empty? tree))
      (begin (display "(")
             (print-branch (tree-left tree) '<)
             (display (tree-entry tree))
             (print-branch (tree-right tree) '>)
             (display ")"))))
#| expample:
> (print-tree (list->tree s1))
(1->((2->(3))<-4->((5)<-6)))

better solution?
|#    

(define (ordered-list->tree lst) ;convert an ordered list to a balanced tree
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size 
               (quotient (- n 1) 2)))
          (let ((left-result 
                 (partial-tree 
                  elts left-size)))
            (let ((left-tree 
                   (car left-result))
                  (non-left-elts 
                   (cdr left-result))
                  (right-size 
                   (- n (+ left-size 1))))
              (let ((this-entry 
                     (car non-left-elts))
                    (right-result 
                     (partial-tree 
                      (cdr non-left-elts)
                      right-size)))
                (let ((right-tree 
                       (car right-result))
                      (remaining-elts 
                       (cdr right-result)))
                  (cons (make-tree left-tree
                                   this-entry
                                   right-tree)
                        remaining-elts))))))))
  (car (partial-tree 
        lst (length lst))))
;; this inner procedure "partial-tree" takes the first n items of a list
;; and makes it into a balanced tree, by the "Divide and Conquer" strategy:
;; take the first n % 2 items and make it into a b-tree as the left
;; tree, take the first item of the rest items as the entry, and take the
;; rest (n - 1 - n%2) items to make the right tree. Thus the resulting tree
;; is still a b-tree. Then it packages the b-tree with the rest of the list
;; (which will be used by the upper layer of recursion) by using cons and
;; return the result. By recursively appying the DaC strategy, the procedure
;; will at last meet the base case: n = 0, which just requires combining '()
;; with the list. The outer procedure "od-list->b-tree" just need to use car
;; to retrieve the b-tree.

#|
;;;this is exactly the same idea as the previously defined procedure "sort"

(define (list->tree lst)
  ;take the car of LST as the top entry of the bnary tree
  (define (divide x lst)
    ;I can just use filter to select the elements smaller than x as well as those bigger than x,
    ;but it's inefficient to traverse the list twice
    (define (iter left right lst)
      (cond ((null? lst) (list left right))
            ((bigger? x (car lst))
             (iter (cons (car lst) left)
                   right
                   (cdr lst)))
            (else
             (iter left
                   (cons (car lst) right)
                   (cdr lst)))))
    (iter nil nil lst))
  (if (null? lst)
      nil 
      (let ((divided-list (divide (car lst) (cdr lst))))
        (let ((entry (car lst))
              (left-list (car divided-list))
              (right-list (cadr divided-list)))
          (make-tree (list->tree left-list)
                     entry
                     (list->tree right-list))))))
|#

(define (list->tree lst)
  (ordered-list->tree (sort lst ascii-compare)))

(define (tree->list tree)
  (if (empty? tree)
      nil
      (append (tree->list (tree-left tree))
              (list (tree-entry tree))
              (tree->list (tree-right tree)))))

(define (element-of-tree? x tree)
  (cond ((empty? tree) #f)
        ((eq? x (tree-entry tree))
         #t)
        ((bigger? x (tree-entry tree))
         (element-of-tree? x (tree-right tree)))
        (else
         (element-of-tree? x (tree-left tree)))))

(define (adjoin-tree x tr)
  (cond ((empty? tr) (make-leaf x))
        ((eq? x (tree-entry tr)) tr)
        ((bigger? x (tree-entry tr))
         (make-tree (tree-left tr)
                    (tree-entry tr)
                    (adjoin-tree x (tree-right tr))))
        (else
         (make-tree (adjoin-tree x (tree-left tr))
                    (tree-entry tr)
                    (tree-right tr)))))

(define (union-tree t1 t2)
  (if (null? t1)
      t2
      (adjoin-tree (tree-entry t1)
                   (union-tree
                    t2
                    (union-tree
                     (tree-left t1)
                     (tree-right t1))))))


;;; for test
(define s '[1 2 6 3 5 4])
(define l '[4 2 3 2 1 3 4 1])
(define ol '[1 2 3 4 5 7 9])
(define (duplicate-list n lst)
  (define (iter result cnt)
    (if (= cnt n)
        result
        (iter (append lst result) (inc cnt))))
  (iter nil 0))
;(define long (duplicate-list 5000 s))


;;; set as a database
;ex 2.66: implement the database as balanced-tree

(define (make-record key data)
  (cons key data))
(define (key record)
  (car record))
(define (data record)
  (cdr record))
(define (bigger-key? r1 r2)
  (bigger? (key r1) (key r2)))

(define empty-db nil)
(define empty-db? null?)
(define (list->db list-of-records)
  (ordered-list->tree (sort
                       list-of-records
                       (lambda (x y)
                         (if (bigger-key? x y) 1 0)))))

(define (first-record db)
  (tree-entry db))
(define (smaller-key-records db)
  (tree-left db))
(define (bigger-key-records db)
  (tree-right db))

(define (add-record record db)
  (if (empty-db? db)
      (make-leaf record)
      (let ((new-key (key record))
            (this-key (key (first-record db)))
            (smaller (smaller-key-records db))
            (bigger (bigger-key-records db)))
        (cond ((eq? new-key this-key)
               (error "this key already exists: ADD-RECORD"
                      new-key))
              ((bigger? new-key this-key)
               (make-tree smaller (first-record db)
                          (add-record record bigger)))
              (else
               (make-tree (add-record record smaller)
                          (first-record db) bigger))))))
    
(define (lookup given-key db)
  (if (empty-db? db)
      #f
      (let ((this-key (key (first-record db))))
        (cond ((eq? given-key this-key)
               (first-record db))
              ((bigger? given-key this-key)
               (lookup given-key (bigger-key-records db)))
              (else
               (lookup given-key (smaller-key-records db)))))))

;;; for test
(define my-db (list->db (list (cons 's 434) (cons 'v "CTZ") (cons 'a 666)
                              (cons 'b "f8ho8") (cons 'w 321) (cons 'r 'f))))