#lang racket
(require "sicp-lang.rkt" "util.rkt")


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
(define (element-of-redupset? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-redupset? x (cdr set)))))

(define (adjoin-redupset x set)
  (cons x set))

(define (intersection-redupset s1 s2)
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
                  (intersection-redupset (cdr s1) left))))))

(define (union-redupset s1 s2)
  (append s1 s2))


;;; sets as ordered lists
(define (element-of-ordset? x set)
  (cond ((null? set) false)
        ((following? (car set) x) false)
        ((eq? x (car set)) true)
        (else (element-of-ordset? x (cdr set)))))

;ex 2.61
(define (adjoin-ordset x set)
  (cond ((null? set) (list x))
        ((following? (car set) x) (cons x set))
        ((eq? x (car set)) set)
        (else (cons (car set)
                    (adjoin-ordset x (cdr set))))))

(define (inter-ordset s1 s2)
  (cond ((or (null? s1) (null? s2)) nil)
        ((following? (car s1) (car s2))
         (inter-ordset s1 (cdr s2)))
        ((eq? (car s1) (car s2))
         (cons (car s1) (inter-ordset (cdr s1) (cdr s2))))
        (else
         (inter-ordset (cdr s1) s2))))

;ex 2.62
(define (union-ordset s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1)) (x2 (car s2)))
           (cond ((following? x1 x2)
                  (cons x2 (union-ordset s1 (cdr s2))))
                 ((eq? x1 x2)
                  (cons x1 (union-ordset (cdr s1) (cdr s2))))
                 (else
                  (cons x1 (union-ordset (cdr s1) s2))))))))


;;; sets as binary trees
(define (make-btset left entry right)
  (list left entry right))
(define (make-leaf entry)
  (make-btset nil entry nil))
(define (btset-left btset)
  (car btset))
(define (btset-entry btset)
  (cadr btset))
(define (btset-right btset)
  (caddr btset))

(define (empty? btset)
  (null? btset))
(define (leaf? btset)
  (and (not (empty? btset))
       (empty? (btset-left btset))
       (empty? (btset-right btset))))
(define (btset? btset)
  (or (empty? btset)
      (and (list? btset)
           (= (length btset) 3)
           (btset? (btset-left btset))
           (btset? (btset-right btset)))))

(define (count-leaves btset)
  (if (empty? btset)
      0
      (+ 1
         (count-leaves (btset-left btset))
         (count-leaves (btset-right btset)))))

;my own way of printing a bbtset
(define (print-btset btset)
  (define (print-branch br dir)
    (if (not (empty? br))
        (if (eq? dir '<)
            (begin (print-btset br) (display "<-"))
            (begin (display "->") (print-btset br)))))
  (if (not (empty? btset))
      (begin (display "(")
             (print-branch (btset-left btset) '<)
             (display (btset-entry btset))
             (print-branch (btset-right btset) '>)
             (display ")"))))
#| expample:
> (print-btset (list->btset s1))
(1->((2->(3))<-4->((5)<-6)))

better solution?
I defined a better procedure in "print-tree.rkt"
|#

(define (ordset->btset lst) ;convert an ordered list to a balanced btset
  (define (partial-btset elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size
               (quotient (- n 1) 2)))
          (let ((left-result
                 (partial-btset
                  elts left-size)))
            (let ((left-btset
                   (car left-result))
                  (non-left-elts
                   (cdr left-result))
                  (right-size
                   (- n (+ left-size 1))))
              (let ((this-entry
                     (car non-left-elts))
                    (right-result
                     (partial-btset
                      (cdr non-left-elts)
                      right-size)))
                (let ((right-btset
                       (car right-result))
                      (remaining-elts
                       (cdr right-result)))
                  (cons (make-btset left-btset
                                   this-entry
                                   right-btset)
                        remaining-elts))))))))
  (car (partial-btset
        lst (length lst))))

;; this inner procedure "partial-btset" takes the first n items of a list
;; and makes it into a balanced btset, by the "Divide and Conquer" strategy:
;; take the first n % 2 items and make it into a b-btset as the left
;; btset, take the first item of the rest items as the entry, and take the
;; rest (n - 1 - n%2) items to make the right btset. Thus the resulting btset
;; is still a b-btset. Then it packages the b-btset with the rest of the list
;; (which will be used by the upper layer of recursion) by using cons and
;; return the result. By recursively appying the DaC strategy, the procedure
;; will at last meet the base case: n = 0, which just requires combining '()
;; with the list. The outer procedure "od-list->b-btset" just need to use car
;; to retrieve the b-btset.


#|
(define (list->btset lst)
  ;take the car of LST as the top entry of the bnary btset
  (define (divide x lst)
    ;I can just use filter to select the elements smaller than x as well as those bigger than x,
    ;but it's inefficient to traverse the list twice
    (define (iter left right lst)
      (cond ((null? lst) (list left right))
            ((following? x (car lst))
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
          (make-btset (list->btset left-list)
                     entry
                     (list->btset right-list))))))
|#

(define (set->btset lst)
  (ordset->btset (sort-literal lst)))

(define (btset->ordset btset)
  (if (empty? btset)
      nil
      (append (btset->ordset (btset-left btset))
              (list (btset-entry btset))
              (btset->ordset (btset-right btset)))))

(define (element-of-btset? x btset)
  (cond ((empty? btset) #f)
        ((eq? x (btset-entry btset))
         #t)
        ((following? x (btset-entry btset))
         (element-of-btset? x (btset-right btset)))
        (else
         (element-of-btset? x (btset-left btset)))))

(define (adjoin-btset x tr)
  (cond ((empty? tr) (make-leaf x))
        ((eq? x (btset-entry tr)) tr)
        ((following? x (btset-entry tr))
         (make-btset (btset-left tr)
                    (btset-entry tr)
                    (adjoin-btset x (btset-right tr))))
        (else
         (make-btset (adjoin-btset x (btset-left tr))
                    (btset-entry tr)
                    (btset-right tr)))))

(define (union-btset t1 t2)
  (if (null? t1)
      t2
      (adjoin-btset (btset-entry t1)
                   (union-btset
                    t2
                    (union-btset
                     (btset-left t1)
                     (btset-right t1))))))


;;; for test
(define s '[1 2 6 3 5 4])
(define l '[4 2 3 2 1 3 4 1])
(define ol '[1 2 3 4 5 7 9])


;;; set as a database
;ex 2.66: implement the database as balanced-btset

(define (make-record key data)
  (cons key data))
(define (key record)
  (car record))
(define (info record)
  (cdr record))
(define (following-record? r1 r2)
  (following? (key r1) (key r2)))

(define empty-db nil)
(define empty-db? null?)
(define (list->db list-of-records)
  (ordset->btset (sort
                       list-of-records
                       (lambda (x y)
                         (if (following-record? x y) 1 0)))))

(define (first-record db)
  (btset-entry db))
(define (smaller-key-records db)
  (btset-left db))
(define (bigger-key-records db)
  (btset-right db))

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
              ((following? new-key this-key)
               (make-btset smaller (first-record db)
                          (add-record record bigger)))
              (else
               (make-btset (add-record record smaller)
                          (first-record db) bigger))))))

(define (lookup given-key db)
  (if (empty-db? db)
      #f
      (let ((this-key (key (first-record db))))
        (cond ((eq? given-key this-key)
               (first-record db))
              ((following? given-key this-key)
               (lookup given-key (bigger-key-records db)))
              (else
               (lookup given-key (smaller-key-records db)))))))

;;; for test
(define my-db (list->db (list (cons 's 434) (cons 'v "CTZ") (cons 'a 666)
                              (cons 'b "f8ho8") (cons 'w 321) (cons 'r 'f))))


;;; Huffman encoding btset
(define frequency-list
  (list (cons 'A 6) (cons 'B 3) (cons 'C 4) (cons 'D 4)
        (cons 'E 7) (cons 'F 1) (cons 'G 1) (cons 'H 2)))


;; my own huffman-btset implementation

(define (symb node)
  (key (btset-entry node)))
(define (freq node)
  (info (btset-entry node)))


(define (print lst) ;for debugging
  (for-each (lambda (item)
              (if (and (btset? item) (not (null? item)))
                  (display (btset-entry item))
                  (display item)))
            lst))


(define (make-huffman-btset freq-list)
  (define (merge-2lowest freq-list)
    (define (merge tr1 tr2)
      (define (symb-set node)
        (if (leaf? node)
            (list (symb node))
            (symb node)))
      (make-btset tr1
                 (make-record
                  (append (symb-set tr1) (symb-set tr2))
                  (+ (freq tr1) (freq tr2)))
                 tr2))
    (define (iter rec1 rec2 checked rest)
      (define (lower-freq? rec1 rec2)
        (< (freq rec1) (freq rec2)))
      (cond ((null? rest)
             (cons (merge rec1 rec2)
                   checked))
            ((lower-freq? (car rest) rec1)
             (iter (car rest) rec2 (cons rec1 checked) (cdr rest)))
            ((lower-freq? (car rest) rec2)
             (iter rec1 (car rest) (cons rec2 checked) (cdr rest)))
            (else
             (iter rec1 rec2 (cons (car rest) checked) (cdr rest)))))
    (define (ensure-btset record)
      (if (btset? record)
          record
          (make-leaf record)))
    (let ((btset-list (map ensure-btset freq-list)))
      (iter (car btset-list) (cadr btset-list) nil (cddr btset-list))))
  (define (iter lst)
    ;(print lst) (newline)
    (if (null? (cdr lst))
        (car lst)
        (iter (merge-2lowest lst))))
  (if (< (length freq-list) 2)
      (error "too few records in the list: MAKE-HUFFMAN-TREE"
             freq-list)
      (iter freq-list)))

(define my-huffbtset (make-huffman-btset frequency-list))


(define (encodings huffman-btset)
  ;(print huffman-btset) (newline)
  (define (add-last bit bin)
    (string-append bin bit))
  (if (leaf? huffman-btset)
      (list (make-record (symb huffman-btset) ""))
      (append (map (lambda (rec)
                     (make-record
                      (key rec)
                      (add-last
                       (info rec) "0")))
                   (encodings (btset-left huffman-btset)))
              (map (lambda (rec)
                     (make-record
                      (key rec)
                      (add-last
                       (info rec) "1")))
                   (encodings (btset-right huffman-btset))))))

;; decode a code encoded by a huffman btset
(define (decode huffman-btset code)
  (define (choose-branch node bit)
    (cond ((char=? bit #\0)
           (btset-left node))
          ((char=? bit #\1)
           (btset-right node))
          (else (error
                 "invalid code: DECODE"
                 code))))
  (define (iter node code)
    (if (equal? code "")
        nil
        (let ((first-bit (string-ref code 0))
              (rest-bits (substring code 1)))
          (let ((next-branch
                 (choose-branch node first-bit)))
            (cond ((leaf? next-branch)
                   (cons (symb next-branch)
                         (iter huffman-btset
                               rest-bits)))
                  (else
                   (iter next-branch
                         rest-bits)))))))
  (iter huffman-btset code))

#| test result:
> (decode my-huffbtset "00111011011")
(H E A D)
|#