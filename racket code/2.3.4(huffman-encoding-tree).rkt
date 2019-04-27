#lang sicp

;;; the data structure implementing huffman encoding tree

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))


;;; methods

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (encode message tree)
  (define (encode-symbol symb tree)
    (cond ((leaf? tree)
           nil)
          ((member
            symb
            (symbols
             (left-branch tree)))
           (cons 0
                 (encode-symbol
                  symb
                  (left-branch tree))))
          ((member
            symb
            (symbols
             (right-branch tree)))
           (cons 1
                 (encode-symbol
                  symb
                  (right-branch tree))))
          (else
           (error "symbol not found: ENCODE"
                  symb))))
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))


;;; generate a huffman tree from a ordered set of pairs of symbol and frequency

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs) ; constructs an ordered set of leaves
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

;; ex 2.67
(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
#|
> (decode sample-message sample-tree)
(A D A B B C A)
|#

;; ex 2.69
(define (generate-huffman-tree pairs)
  (define (successive-merge set)
    (cond ((null? set) nil)
          ((= (length set) 1)
           (car set))
          (else
           (let ((first (car set))
                 (second (cadr set))
                 (rest (cddr set)))
             (successive-merge
              (adjoin-set
               (make-code-tree first second)
               rest))))))
  (successive-merge 
   (make-leaf-set pairs)))

;; ex 2.70
(define rock-alphabet
  '((A    2)    (NA  16)
    (BOOM 1)    (SHA  3)
    (GET  2)    (YIP  9)
    (JOB  2)    (WAH  1)))
(define rock-tree
  (generate-huffman-tree rock-alphabet))
(define code-of-a-song
  (encode
   '(GET A JOB
     SHA NA NA NA NA NA NA NA NA

     GET A JOB
     SHA NA NA NA NA NA NA NA NA

     WAH YIP YIP YIP YIP 
     YIP YIP YIP YIP YIP
     SHA BOOM)
   rock-tree))
;; ??? How to deal with the cases of the letters...
;; So I changed them all into capital letters...
