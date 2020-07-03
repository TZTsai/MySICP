#lang racket
(require
 ;"sicp-lang.rkt"
 "util.rkt"
 (except-in "math.rkt" prime?)
 "3.5.1_stream.rkt"
 "3.5.2_infinite-stream.rkt")

;; utils
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))


;; use stream to express the newton's
;; method to computer sqrt
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


;; a series to compute PI
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (neg-stream (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;; (display-partial-stream pi-stream 10)


;; Euler's transform to accelerate the convergence
;; of alternating series
(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)]
        [s2 (stream-ref s 2)])
    (cons-stream
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

;; (define acc-pi-stream (euler-transform pi-stream))
;; (display-partial-stream acc-pi-stream 10)

;; euler-transform can be applied iteratively
;; to accelerate even further
(define (make-tableau transform s)
  (cons-stream s
        (make-tableau transform
                      (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;; (define supacc-pi-stream
;;   (accelerated-sequence euler-transform pi-stream))
;; (display-partial-stream supacc-pi-stream 10)


;; Ex 3.64
(define (stream-limit s tolerance)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)])
    (if (< (abs (- s0 s1)) tolerance)
        s1
        (stream-limit (stream-cdr s) tolerance))))
; test
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
;; (sqrt 2 0.00001)


;; Ex 3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (neg-stream (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))
;; (display-partial-stream ln2-stream 8)

;; accelerated streams
;; (newline)
;; (display-partial-stream (euler-transform ln2-stream) 8)
;; (newline)
;; (display-partial-stream (accelerated-sequence euler-transform ln2-stream) 8)



;; infinite streams of pairs
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs pos-integers pos-integers))

(define prime-sum-pairs
  (stream-filter (lambda (p)
                   (prime? (+ (car p) (cadr p))))
                 int-pairs))

;; Ex 3.66
;; Suppose there are N1(i,j) pairs preceding (i,j). If i>1, then
;; there are N1(i,j)/2 + 1 pairs whose first element is 1. Let
;; N2(i,j) = N1(i,j)/2 - 1.
;; Generally, let Nk+1(i,j) = Nk(i,j)/2 - 1, denoting the number
;; of pairs preceding (i,j) such that their first elements are
;; greater than k...
;;
;; Another approach:
;; The rule is that the interval between the pairs whose first
;; element is k is 2^k pairs in the stream, except the interval
;; between the first and the second pairs, which is 2^(k-1). It
;; can be proved using mathematical induction. Besides, it is
;; observed that (i,1) lies at the postion of 1+2+...+2^(i-1)
;; = 2^i - 1. Hence, there are 2^i-1+2^(i-1)+(j-i-1)*2^i-1 =
;; (2j-2i+1)*2^(i-1) - 2 pairs preceding (i,j), if j>i; else
;; the answer is 2^i-2.

(define (prec-pairs-num i j)
  (if (= i j)
      (- (expt 2 i) 2)
      (- (* (+ (* 2 j) (* -2 i) 1)
            (expt 2 (- i 1)))
         2)))

;; test
;; (display-partial-stream
;;  (stream-map (lambda (p) (prec-pairs-num (car p) (cadr p)))
;;              (pairs pos-integers pos-integers))
;;  30)

;; Hence, there are 100*2-3=197 pairs preceding (1, 100),
;; 3*2^98-2 pairs preceding (99, 100), 2^99-2 pairs preceding
;; (100, 100).


;; Ex 3.67
(define (all-pairs s1 s2)
  (cons-stream (list (stream-car s1)
                     (stream-car s2))
               (interleave
                (stream-map
                 (lambda (n)
                   (list (stream-car s1) n))
                 (stream-cdr s2))
                (all-pairs
                 (stream-cdr s1)
                 s2))))

(define all-int-pairs
  (all-pairs pos-integers
             pos-integers))

;; (display-partial-stream all-int-pairs 20)


;; Ex 3.68
(define (simp-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (simp-pairs (stream-cdr s) (stream-cdr t))))

;; test:
;; (display-partial-stream (simp-pairs pos-integers pos-integers) 20)
;; It turns out that the evaluation will loop infinitely, because
;; there is no delay in the recursive evaluation.


;; Ex 3.69
(define (triples-slow s t u)
  (cons-stream
   (list (stream-car s)
         (stream-car t)
         (stream-car u))
   (interleave
    (stream-map
     (lambda (p)
       (cons (stream-car s) p))
     (stream-cdr (pairs t u)))
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))

(define (triples s t u)
  (define (rec si i pairs-tu imax-stream)
    (cons-stream (cons (stream-car si)
                       (stream-car pairs-tu))
                 (if (= i (stream-car imax-stream))
                     (rec s 1 (stream-cdr pairs-tu)
                          (stream-cdr imax-stream))
                     (rec (stream-cdr si) (add1 i)
                          pairs-tu imax-stream))))
  (rec s 1 (pairs t u) (stream-map car int-pairs)))

;; someone on the sicp-solutions website said the
;; first impl is slow, but by my own test, these
;; two impls do not vary much in efficiency

;; test
;; (display-partial-stream
;;  (triples pos-integers pos-integers pos-integers)
;;  30)

(define (Pythagorean? a b c)
  (= (+ (square a) (square b))
     (square c)))

(define Pythagorean-triples
  (stream-filter
   (lambda (t)
     (apply Pythagorean? t))
   (triples pos-integers pos-integers pos-integers)))

;; (display-partial-stream Pythagorean-triples 4)
;; (define (test n)
;;   (stream-ref Pythagorean-triples n))
;; ((timed test) 5)


;; Ex 3.70
(define (merge-weighted ps1 ps2 w)
  (cond ((stream-null? ps1) ps2)
        ((stream-null? ps2) ps1)
        (else
         (let ((w1 (apply w (stream-car ps1)))
               (w2 (apply w (stream-car ps2))))
           (cond ((< w1 w2)
                  (cons-stream
                   (stream-car ps1)
                   (merge-weighted
                    (stream-cdr ps1) ps2 w)))
                 ((> w1 w2)
                  (cons-stream
                   (stream-car ps2)
                   (merge-weighted
                    ps1 (stream-cdr ps2) w)))
                 (else
                  ; Ex 3.71
                  ; (display "Ramahujan number found: ")
                  ; (displayln w1)
                  ;
                  (cons-stream
                   (stream-car ps1)
                   (cons-stream
                    (stream-car ps2)
                    (merge-weighted
                     (stream-cdr ps1)
                     (stream-cdr ps2)
                     w)))))))))

(define (ordered-pairs s t w)
  (cons-stream (list (stream-car s)
                     (stream-car t))
               (merge-weighted
                (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (ordered-pairs
                 (stream-cdr s) (stream-cdr t) w)
                w)))

(define ordered-intpairs
  (ordered-pairs pos-integers
                 pos-integers
                 +))

;; (display-partial-stream ordered-intpairs 20)

(define stream-b
  (stream-filter (lambda (p)
                   (let ([i (car p)]
                         [j (cadr p)])
                     (not (or (divides? 2 i)
                              (divides? 3 i)
                              (divides? 5 i)
                              (divides? 2 j)
                              (divides? 3 j)
                              (divides? 5 j)))))
                 (ordered-pairs pos-integers
                                pos-integers
                                (lambda (i j)
                                  (+ (* 2 i)
                                     (* 3 j)
                                     (* 5 i j))))))

;; (display-partial-stream stream-b 20)


;; Ex 3.71
;; I just add two lines in the def of
;; merge-weighted to print the weight
;; when it enters the case that two
;; consequent pairs have the same weight.
(define cubesum-ordered-intpairs
  (ordered-pairs pos-integers
                 pos-integers
                 (lambda (i j)
                   (+ (* i i i)
                      (* j j j)))))

;; test: (stream-ref cubesum-ordered-intpairs 1000)
;; result:
;; Ramahujan number found: 1729
;; Ramahujan number found: 4104
;; Ramahujan number found: 13832
;; Ramahujan number found: 20683
;; Ramahujan number found: 32832
;; Ramahujan number found: 39312
;; Ramahujan number found: 40033
;; Ramahujan number found: 46683
;; Ramahujan number found: 64232
;; Ramahujan number found: 65728


;; Ex 3.72
;; I try another approach than the prev Ex here.
(define (ordered-pairs-with-weight s t w)
  (define (tag-weight p)
    (cons (apply w p) p))
  (cons-stream (tag-weight (list (stream-car s)
                                 (stream-car t)))
               (merge-weighted
                (stream-map (lambda (x)
                              (tag-weight
                               (list (stream-car s) x)))
                            (stream-cdr t))
                (ordered-pairs-with-weight
                 (stream-cdr s) (stream-cdr t) w)
                (lambda (w i j) w))))

(define sqsum-ordered-intpairs
  (ordered-pairs-with-weight
   pos-integers pos-integers sq-sum))

(define (filter-identical-weight-pairs s m)
  (define (iter s i)
    (let ([s1 (stream-car s)]
          [s2 (stream-car (stream-cdr s))])
      (if (= (car s1) (car s2))
          (if (= i (- m 1))
              (cons-stream (car s1)
                           (iter (stream-cdr s) 1))
              (iter (stream-cdr s) (+ 1 i)))
          (iter (stream-cdr s) 1))))
  (iter s 1))

(define ramanujan-nums
  (filter-identical-weight-pairs
   (ordered-pairs-with-weight
    pos-integers pos-integers
    (lambda (i j) (+ (* i i i)
                     (* j j j))))
   2))

;; the first 6 ramanujan nums
;; (display-partial-stream ramanujan-nums 5)

(define sqsum-3way-nums
  (filter-identical-weight-pairs
   sqsum-ordered-intpairs 3))

;; (display-partial-stream sqsum-3way-nums 5)



;; streams as signals

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; (display-partial-stream (integral pos-integers 0 1) 10)


;; Ex 3.73
;; we can use this proc to describe an RC circuit
;; the math formula for an RC series circuit is
;; v(t) = v(0) + 1/C Integral[i(r), {r, 0, t}] + R i(t)
(define (RC R C dt)
  (define (v-out i v0)
    (add-streams
     (scale-stream (integral i v0 dt)
                   (/ 1 C))
     (stream-map (lambda (it)
                   (+ v0 (* R it)))
                 i)))
  v-out)

; test
;; (define rc1 (RC 5 0.032 0.00001))
;; (display-partial-stream (rc1 (stream-map sin pos-integers) 2.4) 10)


;; Ex 3.74
(define (sign-change-detector v2 v1)
  (if (or (and (< v1 0) (< v2 0))
          (and (>= v1 0) (>= v2 0)))
      0
      (if (< v1 0) 1 -1)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream)
                         last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define (zero-crossings sense-data)
  (stream-map sign-change-detector
              (stream-cdr sense-data)
              sense-data))

; test
;; (define sense-data (cons-stream 1 (cons-stream 0 (neg-stream sense-data))))
;; (define zc (make-zero-crossings (stream-cdr sense-data) (stream-car sense-data)))
;; (define zc* (zero-crossings sense-data))


;; Ex 3.75
(define (smooth-and-make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (smooth-and-make-zero-crossings
                  (stream-cdr input-stream) (stream-car input-stream) avpt))))


;; Ex 3.76
(define (smooth s)
  (stream-map average s (stream-cdr s)))

(define (smooth-and-make-zero-crossings* input-stream)
  (let ([smoothed-ins (smooth input-stream)])
    (stream-map sign-change-detector
                (stream-cdr smoothed-ins)
                smoothed-ins)))
