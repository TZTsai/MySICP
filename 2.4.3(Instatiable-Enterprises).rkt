#lang sicp
(#%require "util.rkt" "data-structures.rkt")

(define (make-record name address salary)
  (list name address salary))

(define (get-name record)
  (car record))
(define (get-address record)
  (cadr record))
(define (get-salary record)
  (caddr record))

(define (make-file set-of-records division)
  (attach-tag division set-of-records))

(define (get-file division)
  (get 'file division))

(define (get-record name file)
  (apply-generic 'lookup name file))

(define (find-employee-record name)
  (filter identity
          (map (lambda (file)
                 (get-record name file))
               (map (lambda (division)
                      (get-file division))
                    division-list))))

(define division-list nil)
(define (add-division records division lookup)
  (put 'file division (make-file records division))
  (put 'lookup (list #f division) lookup)
  (set! division-list (cons division division-list))
  'done)

(define (add-division1) ; set-of-records implemented as common set
  (define (lookup name set-of-records)
    (assoc name set-of-records))
  (add-division division1-records 'division1 lookup))

(define (add-division2)
  (add-division division2-records 'division2 lookup)) ; I directly import the LOOKUP prodcedure for dict in "data-structures.rkt"

(define (add-division3)
  (define (lookup name set-of-records)
    (if (null? set-of-records)
        false
        (let ((this-name (get-name (car set-of-records))))
          (cond ((smaller? name this-name) false)
                ((equal? name this-name) (car set-of-records))
                (else (lookup name (cdr set-of-records)))))))
  (add-division division3-records 'division3 lookup))

;;;for test
(define division1-records
  (list (make-record 'alpha "#300 Rd.A" 30000)
        (make-record 'beta "#245 Rd.C" 25000)
        (make-record 'gamma "#466 Rd.B" 34000)))
(define division2-records
  (list->dict
   (list (make-record 'LongY "#216 Rd.Shannon" 40000)
         (make-record 'MK "#160 Rd.Newton" 45000)
         (make-record 'Horst "#286 Rd.Gauss" 66600))))
(define division3-records
  (sorting-tool
   (list (make-record 'Hitler "#232 Rd.Dictators" 99999999)
         (make-record 'Stalin "#233 Rd.Dictators" 66666666)
         (make-record 'Churchill "#10 Rd.Downing" 987654321))
   (lambda (r1 r2)
     (literal-compare (get-name r1) (get-name r2)))
   1))
(define (add-all)
  (add-division1)
  (add-division2)
  (add-division3))