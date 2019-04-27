#lang sicp

(define (average a b)
  (* (+ a b) 0.5))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.000001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (fold comb init)
  (define (fast-comb item cnt)
    (cond ((zero? cnt) init)
          ((even? cnt) (fast-comb (comb item item) (/ cnt 2)))
          (else ((fold comb (comb item init)) item (dec cnt)))))
  fast-comb)

(define repeated (fold compose identity))
(define fast-expt (fold * 1))

(define (data-table f min max dx)
  (define (iter x cnt)
    (define (data)
      (begin (display x)
             (display ": ")
             (display (f x))))
    (define (space)
      (if (zero? (remainder cnt 15))
          (newline)
          (display "   ")))
    (cond ((> x max) (newline))
          (else (data)
                (space)
                (iter (+ x dx) (inc cnt)))))   
  (iter min 1))
;;;above are auxiliary procedures

(define (protected-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.000001))
  (define (try guess cnt)
    (let ((next (f guess)))
      (cond ((close-enough? guess next) next)
            ((> cnt 10000) false)
            (else (try next (inc cnt))))))
  (try first-guess 0))

(define (test x n damps) ;test if the computation of the nth root of x can converge using the average damp for damps times
  (define (f y)
    (/ x (expt y (- n 1)))) 
  (let ((damped (repeated average-damp damps)))
    (protected-fixed-point (damped f) 1.0)))

(define (least-damps-by-test x n)
  (define (iter damps)
    (if (test x n damps)
        damps
        (iter (inc damps))))
  (iter 0))

(define (test-data x)
  (data-table (lambda (n)
                (least-damps-by-test x n))
              1 150 1))
#|test result:
> (test-data 100000)
1: 0   2: 1   3: 1   4: 2   5: 2   6: 2   7: 2   8: 3   9: 3   10: 3   11: 3   12: 3   13: 3   14: 3   15: 3
16: 4   17: 4   18: 4   19: 4   20: 4   21: 4   22: 4   23: 4   24: 4   25: 4   26: 4   27: 4   28: 4   29: 4   30: 4
31: 4   32: 5   33: 5   34: 5   35: 5   36: 5   37: 5   38: 5   39: 5   40: 5   41: 5   42: 5   43: 5   44: 5   45: 5
46: 5   47: 5   48: 5   49: 5   50: 5   51: 5   52: 5   53: 5   54: 5   55: 5   56: 5   57: 5   58: 5   59: 5   60: 5
61: 5   62: 5   63: 5   64: 6   65: 6   66: 6   67: 6   68: 6   69: 6   70: 6   71: 6   72: 6   73: 6   74: 6   75: 6
76: 6   77: 6   78: 6   79: 6   80: 6   81: 6   82: 6   83: 6   84: 6   85: 6   86: 6   87: 6   88: 6   89: 6   90: 6
91: 6   92: 6   93: 6   94: 6   95: 6   96: 6   97: 6   98: 6   99: 6   100: 6   101: 6   102: 6   103: 6   104: 5   105: 6
106: 6   107: 6   108: 6   109: 6   110: 6   111: 6   112: 6   113: 6   114: 6   115: 6   116: 6   117: 6   118: 6   119: 6   120: 6
121: 6   122: 6   123: 6   124: 6   125: 6   126: 6   127: 6   128: 7   129: 7   130: 7   131: 7   132: 7   133: 7   134: 7   135: 7
136: 7   137: 7   138: 7   139: 7   140: 7   141: 7   142: 7   143: 7   144: 7   145: 7   146: 7   147: 7   148: 7   149: 7   150: 7
--------------
Observation:
It appears that the least-damps increase by 1 when n reaches the next power of 2.
|#

(define (log2 x) (/ (log x) (log 2)))
(define (nth-root n x)
  (define (least-damps n)
    (floor (log2 n)))
  (define (f y)
    (/ x (expt y (- n 1))))
  (fixed-point ((repeated average-damp (least-damps n)) f)
               1.0))