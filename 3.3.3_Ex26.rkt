#lang racket
(require "sicp-lang.rkt" "util.rkt")

(define (print-list l)
  (cond [(procedure? l)
         (display "(")
         (cond [(null? l) (display "")]
               [(procedure? (car l))
                (print-list (car l))
                (display " . ")
                (print-list (cdr l))]
               [else
                (display (car l))
                (display " . ")
                (print-list (cdr l))])
         (display ")")]
        [else (display l)]))

;; binary tree implementation
(define (make-btree left entry right)
  (list left entry right))
(define (btree-left btree)
  (car btree))
(define (btree-entry btree)
  (cadr btree))
(define (btree-right btree)
  (caddr btree))
(define (make-leaf x)
  (make-btree nil x nil))

(define (btree-assoc key t)
  (if (null? t)
      #f
      (let ([this-entry (btree-entry t)]
            [this-key (car (btree-entry t))]
            [left (btree-left t)]
            [right (btree-right t)])
        (cond [(smaller? key this-key)
               (btree-assoc key left)]
              [(equal? key this-key)
               this-entry]
              [else
               (btree-assoc key right)]))))

(define (btree-add-entry entry t)
  (if (null? t)
      (make-leaf entry)
      (let ([new-key (car entry)]
            [this-entry (btree-entry t)]
            [this-key (car (btree-entry t))]
            [left (btree-left t)]
            [right (btree-right t)])
        (cond [(equal? new-key this-key)
               (error "key exists: ADD-ENTRY" this-key)]
              [(bigger? new-key this-key)
               (make-btree left this-entry
                           (btree-add-entry
                            entry right))]
              [else
               (make-btree (btree-add-entry
                            entry left)
                           this-entry right)]))))


;; imp the abstracted procedures
(define empty-database null)
(define database-assoc btree-assoc)
(define database-add-entry btree-add-entry)

;; set an abstration barrier:
;; empty-database, database-assoc, database-add-entry
;; a table is a pair of a key and a database
;; (the master table's key is specified as '*table*)
;; every entry of the database is either a subtable
;; or a pair of a key and a value
(define (make-table)
  (let ([table (cons '*table* empty-database)])
    (define (lookup keys table)
      (let ([entry (database-assoc (car keys) (cdr table))])
        (if entry
            (if (null? (cdr keys))
                (cdr entry)
                (lookup (cdr keys) entry))
            #f)))
    (define (insert! keys val table)
      (define (new-entry keys val)
        (if (null? (cdr keys))
            (cons (car keys) val)
            (cons (car keys)
                  (database-add-entry
                   (new-entry (cdr keys) val)
                   empty-database))))
      (let ([entry (database-assoc (car keys) (cdr table))])
        (if entry
            (if (null? (cdr keys))
                ;ATTENTION! insert! may directly rewrite the database into val
                (set-cdr! entry val)
                (insert! (cdr keys) val entry))
            (set-cdr! table
                      (database-add-entry
                       (new-entry keys val)
                       (cdr table))))))
    (define (dispatch m)
      (cond [(eq? m 'lookup)
             (lambda (keys) (lookup keys table))]
            [(eq? m 'insert!)
             (lambda (keys val) (insert! keys val table))]
            [(eq? m 'database)
             (cdr table)]
            [else (error "unknown operation: TABLE" m)]))
    dispatch))


;; test
(define T (make-table))
(define put (T 'insert!))
(define get (T 'lookup))
(define (show) (print-list (T 'database)))

;; emulate the answers of an exam
(put '(A a i) 'B)
(put '(A b i) 72)
(put '(A b ii) 12)
(put '(B a) 'C)
(put '(A a ii) 4)
(put '(B b) 'A)
(put '(C a) 'false)
(put '(C b i) 12)
(put '(C b ii) 0)
(define T_A (get '(A)))
;; change an answer:
(put '(A a ii) 6)
;; the content of T_A is also changed
