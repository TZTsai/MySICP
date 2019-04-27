#lang racket
(require "sicp-lang.rkt")
(require "util.rkt")
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


;; ordered sets
(define bigger? following?)
(define smaller? preceding?)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((bigger? (car set) x) false)
        ((smaller? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((bigger? (car set) x) (cons x set))
        ((equal? x (car set)) set)
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2))
      nil
      (let ((x1 (car s1)) (x2 (car s2)))
        (cond ((bigger? x1 x2)
               (intersection-set s1 (cdr s2)))
              ((equal? x1 x2)
               (cons x1 (intersection-set (cdr s1) (cdr s2))))
              (else
               (intersection-set (cdr s1) s2))))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1)) (x2 (car s2)))
           (cond ((bigger? x1 x2)
                  (cons x2 (union-set s1 (cdr s2))))
                 ((eq? x1 x2)
                  (cons x1 (union-set (cdr s1) (cdr s2))))
                 (else
                  (cons x1 (union-set (cdr s1) s2))))))))


;; binary trees
(define (make-btree left entry right)
  (list left entry right))
(define (make-bleaf entry)
  (make-btree nil entry nil))
(define (btree-left btree)
  (car btree))
(define (btree-entry btree)
  (cadr btree))
(define (btree-right btree)
  (caddr btree))

(define (bleaf? btree)
  (and (null? (btree-left btree))
       (null? (btree-right btree))))
(define (btree? item)
  (define btree/null?
    (lambda (x)
      (or (btree? x)
          (empty? x))))
  (and (list? item)
       (= (length item) 3)
       (btree/null? (btree-left item))
       (btree/null? (btree-right item))))

(define (ordered-set->btree set) ;convert an ordered set to a balanced btree
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
                  (cons (make-btree left-tree
                                    this-entry
                                    right-tree)
                        remaining-elts))))))))
  (car (partial-tree 
        set (length set))))

(define (set->btree set)
  (ordered-set->btree
   (sort-num set)))

(define (adjoin-btree x tr)
  (cond ((empty? tr) (make-bleaf x))
        ((eq? x (btree-entry tr)) tr)
        ((bigger? x (btree-entry tr))
         (make-btree (btree-left tr)
                     (btree-entry tr)
                     (adjoin-btree x (btree-right tr))))
        (else
         (make-btree (adjoin-btree x (btree-left tr))
                     (btree-entry tr)
                     (btree-right tr)))))

(define (merge-btree t1 t2)
  (cond ((null? t2) t1)
        ((null? t1) t2)
        (else
         (let ((x1 (btree-entry t1))
               (x2 (btree-entry t2))
               (l1 (btree-left t1))
               (l2 (btree-left t2))
               (r1 (btree-right t1))
               (r2 (btree-right t2)))
           (cond ((bigger? x1 x2)
                  (merge-btree
                   r2
                   (make-btree
                    (adjoin-btree
                     x2
                     (merge-btree
                      l1 l2))
                    x1
                    r1)))
                 ((equal? x1 x2)
                  (make-btree
                   (merge-btree
                    l1 l2)
                  x1
                  (merge-btree
                   r1 r2)))
                 (else
                  (merge-btree t2 t1)))))))

(define (btree-map proc btree)
  (if (null? btree)
      null
      (make-btree
       (btree-map proc (btree-left btree))
       (proc (btree-entry btree))
       (btree-map proc (btree-right btree)))))

(define (btree-layer bt n)
  (cond ((null? bt) null)
        ((= n 0) (list (btree-entry bt)))
        (else (append
               (btree-layer
                (btree-left bt)
                (- n 1))
               (btree-layer
                (btree-right bt)
                (- n 1))))))
      
(define (print-btree btree)
  (define (size exp)
    (string-length (->string exp)))
  (define (half size)
    (quotient (+ size 1) 2))
  (define (make-space total left right)
    (list total left right))
  (define (get-space btree)
    (if (null? btree)
        (list 2 0 0)
        (car (btree-entry btree))))
  (define (total space)
    (car space))
  (define (left-arm space)
    (half (cadr space)))
  (define (left-blank space)
    (- (cadr space) (left-arm space)))
  (define (right-arm space)
    (half (caddr space)))
  (define (right-blank space)
    (- (caddr space) (right-arm space)))
  (define (alloc-space btree)
    (if (null? btree)
        null
        (letrec ([left (alloc-space (btree-left btree))]
                 [right (alloc-space (btree-right btree))]
                 [entry (btree-entry btree)])
          (let ([l-spc (total (get-space left))]
                [r-spc (total (get-space right))])
            (make-btree
             left
             (cons (make-space
                    (+ l-spc r-spc)
                    l-spc r-spc)
                   entry)
             right)))))
  (define (layers btree)
    (define (partial-layers init-depth)
      (if (null? (btree-layer btree init-depth))
          null
          (cons (btree-layer btree init-depth)
                (partial-layers (+ init-depth 1)))))
    (partial-layers 0))
  (define (layer->string layer)
    (define (entry->string entry)
      (let ([space (car entry)])
        (string-append
         (make-string (left-blank space) #\ )
         (make-string (- (left-arm space)
                         (quotient (add1 (size (cdr entry))) 2))
                      #\-)
         (->string (cdr entry))
         (make-string (- (right-arm space)
                         (quotient (size (cdr entry)) 2))
                      #\-)
         (make-string (right-blank space) #\ ))))
    (string-append
     (accumulate string-append ""
                 (map entry->string layer))
     "\n"))
  (let ((lines (layers (alloc-space btree))))
    (for-each display (map layer->string lines))
    (map layer->string lines)))


;; dicts (using btree)
(define (make-entry key content)
  (cons key content))
(define (entry-key entry)
  (car entry))
(define (entry-content entry)
  (cdr entry))

(define (list->dict entry-list)
  (ordered-set->btree
   (sort
    entry-list
    (lambda (x y)
      (if (bigger? (entry-key x)
                   (entry-key y))
          1 0)))))

(define (add-entry entry dict)
  (if (null? dict)
      (make-bleaf entry)
      (let ((new-key (entry-key entry))
            (this-key (entry-key (btree-entry dict)))
            (this-entry (btree-entry dict))
            (smaller (btree-left dict))
            (bigger (btree-right dict)))
        (cond ((equal? new-key this-key)
               (display "Warning: overwriting entry")
               (make-btree smaller entry bigger))
              ((bigger? new-key this-key)
               (make-btree smaller this-entry
                           (add-entry entry bigger)))
              (else
               (make-btree (add-entry entry smaller)
                           this-entry bigger))))))

(define (lookup given-key dict)
  (if (null? dict)
      #f
      (let ((this-key (entry-key (btree-entry dict))))
        (cond ((eq? given-key this-key)
               (btree-entry dict))
              ((bigger? given-key this-key)
               (lookup given-key (btree-right dict)))
              (else
               (lookup given-key (btree-left dict)))))))


;; tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

;; 2D tables
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))