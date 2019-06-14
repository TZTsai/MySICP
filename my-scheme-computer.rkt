#lang racket
(require "sicp-lang.rkt")

;; utils
(define (bit? x)
  (and (number? x)
       (or (= x 0)
           (= x 1))))

(define (filter pred list)
  (cond [(null? list) null]
        [(pred (car list))
         (cons (car list)
               (filter pred (cdr list)))]
        [else (filter pred (cdr list))]))

(define (enum n)
  (define (iter list n)
    (if (< n 0)
        list
        (iter (cons n list) (- n 1))))
  (iter null (- n 1)))

(define (for-each-pair fn l1 l2)             ;fn is a 2-argument procedure
  (cond [(and (not (null? l1))
              (not (null? l2)))
         (fn (car l1) (car l2))
         (for-each-pair fn (cdr l1) (cdr l2))]))

(define (string->numlist string)
  (map (lambda (char) (- (char->integer char) 48))
       (string->list string)))

(define (partial-list list start end)
  (define (chop list end)
    (if (or (null? list) (< end 0))
        null
        (cons (car list) (chop (cdr list) (- end 1)))))
  (if (null? list)
      null
      (if (= start 0)
          (chop list end)
          (partial-list (cdr list) (- start 1) (- end 1)))))

(define (bits->int bits)
  (define (iter bits int)
    (if (null? bits)
        int
        (iter (cdr bits)
              (+ (* 2 int)
                 (car bits)))))
  (iter bits 0))

;; implementation of a wire
(define (make-wire)
  (let ([bit 0]
        [actions null]
        [free? #t])
    (define (set-bit! b)
      (cond [(not (= bit b))
             (set! bit b)
             (for-each (lambda (proc) (proc))
                       actions)]))
    (define (add-action! action)
      (set! actions (cons action actions)))
    (define (dispatch m)
      (cond [(bit? m) (set-bit! m)]
            [(eq? m 'get) bit]
            [(eq? m 'free?) free?]
            ;these two messages below should only be passed in "connect!"
            [(eq? m 'add-action!) add-action!]
            [(eq? m 'protect!) (set! free? #f)]
            [else (error "unknown message: WIRE" m)]))
    dispatch))

(define (get-bit wire)
  (wire 'get))

(define clk (make-wire))
;; clk can only be mutated through "next-cycle"
(define (next-cycle)
  (clk (- 1 (get-bit clk))))
;; actually this is half of a cycle
;; but the function is the same

;; now we have a proc to connect a logic chip
(define (connect! wires-in wire-out logic)
  (define (free-wire? wire)
    (wire 'free?))
  (define (action)
    (let ([inputs (map get-bit wires-in)])
      (wire-out (apply logic inputs))))
  (define (add-action! wire)
    ((wire 'add-action!) action))
  (if (free-wire? wire-out)
      (for-each add-action! wires-in)
      (error "connectinng dependent output wire: CONNECT!"))
  (action)             ;thus "wire-out" is set into the new bit
  (wire-out 'protect!) ;thus "wire-out" cannot be connected as output wire again
  'ok)

;; bus
(define (empty-bus)
  (let ([wires null]
        [width 0])
    (define (set-bits! bits)
      (for-each-pair
       (lambda (wire bit)
         (wire bit))
       wires bits))
    (define (dispatch m)
      (cond [(and (number? m) (< m width))
             (list-ref wires m)]
            [(and (list? m) (= (length m) width))
             (set-bits! m)]
            [(string? m)
             (dispatch (string->numlist m))]
            [(eq? m 'set-wires!)
             (lambda (wire-list)
               (set! wires wire-list))]
            [(eq? m 'set-width!)
             (lambda (w)
               (set! width w))]
            [(eq? m 'width) width]
            [(eq? m 'get) wires]
            [(eq? m 'data) (for-each display (map get-bit wires))]
            [else (error "unknown message: BUS" m)]))
    dispatch))

(define (make-bus width)
  (let ([bus (empty-bus)])
    ((bus 'set-wires!)
     (map (lambda (n) (make-wire))
          (enum width)))
    ((bus 'set-width!) width)
    bus))

(define (bus-width bus)
  (bus 'width))

(define (bus-bits bus)
  (map get-bit (bus 'get)))

(define (bus-data bus)
  (bus 'data))

(define (wires->bus wires)
  (let ([bus (empty-bus)])
    ((bus 'set-wires!) wires)
    ((bus 'set-width!) (length wires))
    bus))

(define (split-bus bus start end)
  (wires->bus (partial-list (bus 'get) start end)))

(define (zero width)
  (let ([bus (make-bus width)])
    bus))

(define (one width)
  (let ([bus (make-bus width)])
    ((bus 0) 1)
    bus))

(define zero16 (zero 16))
(define one16 (one 16))
(define wire-true (one16 0))
(for-each (lambda (n)
            ((zero16 n) 'protect!)
            ((one16 n) 'protect!))
          (enum 16))

;; logic gates
(define (not-gate in out)
  (connect! (list in) out
           (lambda (bin) (- 1 bin))))

(define (and-gate a b out)
  (connect! (list a b) out
           (lambda (ba bb)
             (if (= (* ba bb) 1)
                 1 0))))

(define (nor-gate a b out)
  (connect! (list a b) out
           (lambda (ba bb)
             (if (= (+ ba bb) 0)
                 1 0))))

(define (or-gate a b out)
  (connect! (list a b) out
           (lambda (ba bb)
             (if (> (+ ba bb) 0)
                 1 0))))

(define (xor-gate a b out)
  (connect! (list a b) out
           (lambda (ba bb)
             (if (= (+ ba bb) 1)
                 1 0))))

(define (mux a b sel out)
  (let ([nsel (make-wire)]
        [sa (make-wire)]
        [sb (make-wire)])
    (not-gate sel nsel)
    (and-gate a nsel sa)
    (and-gate b sel sb)
    (or-gate sa sb out)))

(define (full-adder cin a b s cout)
  (define (half-adder a b s c)
    (xor-gate a b s)
    (and-gate a b c))
  (let ([s0 (make-wire)]
        [c0 (make-wire)]
        [c1 (make-wire)])
    (half-adder a b s0 c0)
    (half-adder s0 cin s c1)
    (or-gate c0 c1 cout)))

(define (add16 cin A B S cout)
  (if (and (= (bus-width A) 16)
           (= (bus-width B) 16)
           (= (bus-width S) 16))
      (let ([C (make-bus 15)])
        (for-each
         (lambda (n)
           (full-adder
            (if (= n 0) cin (C (- n 1)))
            (A n) (B n) (S n)
            (if (= n 15) cout (C n))))
         (enum 16)))
      (error "wrong bus width: ADD16")))

(define (inc16 IN OUT)
  (if (= (bus-width IN) 16)
      (let ([cin (make-wire)]
            [cout (make-wire)])
        (add16 cin IN one16 OUT cout))
      (error "wrong bus width: INC16")))

(define (mux16 A B sel OUT)
  (for-each
   (lambda (n)
     (mux (A n) (B n) sel (OUT n)))
   (enum 16)))

(define (mux4way16 IN1 IN2 IN3 IN4 SEL OUT)
  (if (and (= (bus-width IN1) 16)
           (= (bus-width IN2) 16)
           (= (bus-width IN3) 16)
           (= (bus-width IN4) 16)
           (= (bus-width SEL) 2))
      (let ([M1 (make-bus 16)]
            [M2 (make-bus 16)])
        (mux16 IN1 IN2 ((SEL 0) 'get) M1)
        (mux16 IN3 IN4 ((SEL 0) 'get) M2)
        (mux16 M1 M2 ((SEL 1) 'get)))
      (error "wrong bus width: MUX4WAY16")))

(define (mux8way16 IN1 IN2 IN3 IN4 IN5 IN6 IN7 IN8 SEL OUT)
  (if (and (= (bus-width IN1) 16)
           (= (bus-width IN2) 16)
           (= (bus-width IN3) 16)
           (= (bus-width IN4) 16)
           (= (bus-width IN5) 16)
           (= (bus-width IN6) 16)
           (= (bus-width IN7) 16)
           (= (bus-width IN8) 16)
           (= (bus-width SEL) 3))
      (let ([M1 (make-bus 16)]
            [M2 (make-bus 16)]
            [SEL2 (split-bus SEL 0 1)])
        (mux4way16 IN1 IN2 IN3 IN4 SEL2 M1)
        (mux4way16 IN5 IN6 IN7 IN8 SEL2 M2)
        (mux16 M1 M2 ((SEL 2) 'get)))
      (error "wrong bus width: MUX4WAY16")))

#|
For efficiency, I write a built-in chip for DFF

(define (DFF in clk out)
  (define (flip-flop r s q nq)
    (nor-gate r nq q)
    (nor-gate s q nq))
  (define (latch in ld out)
    (let ([ni (make-wire)]
          [li (make-wire)]
          [nli (make-wire)]
          [nout (make-wire)])
      (not-gate in ni)
      (and-gate ld in li)
      (and-gate ld ni nli)
      (flip-flop nli li out nout)))
  (let ([nclk (make-wire)]
        [mid (make-wire)])
    (not-gate clk nclk)
    (latch in nclk mid)
    (latch mid clk out)))
|#
(define (DFF in out)
  (define (action)
    (out (get-bit in)))
  ((clk 'add-action!) action))

(define (register IN ld OUT)
  (define (bit in ld out)
    (let ([m (make-wire)])
      (mux out in ld m)
      (DFF m out)))
  (if (and (= (bus-width IN) 16)
           (= (bus-width OUT) 16))
      (for-each (lambda (n) (bit (IN n) ld (OUT n)))
                (enum 16))
      (error "wrong bus width: REGISTER")))

(define (pc IN ld inc rst OUT)
  (if (and (= (bus-width IN) 16)
           (= (bus-width OUT) 16))
      (let ([INC (make-bus 16)]
            [MINC (make-bus 16)]
            [MLD (make-bus 16)]
            [MRST (make-bus 16)])
        (inc16 OUT INC)
        (mux16 OUT INC inc MINC)
        (mux16 MINC IN ld MLD)
        (mux16 MLD zero16 rst MRST)
        (register MRST wire-true OUT))
      (error "wrong bus width: PC")))

;; also a built-in chip
(define (RAM1K IN ld ADR OUT)
  (if (and (= (bus-width IN) 16)
           (= (bus-width ADR) 10)
           (= (bus-width OUT) 16))
      (let ([lds (map (lambda (n) (make-wire))
                      (enum 1024))]
            [address (bits->int (bus-bits ADR))])
        (define (action)
          (for-each
           (lambda (n)
             (let ([this-ld (list-ref lds n)])
               (if (= n address)
                   (this-ld (get-bit ld))
                   (this-ld 0))))
           (enum 1024)))
        ((ld 'add-action!) action)
        (for-each (lambda (n)
                    (((ADR n) 'add-action!) action))
                  (enum 10))
        (for-each (lambda (n)
                    (register IN (list-ref lds n) OUT))
                  (enum 1024)))
      (error "wrong bus width: RAM16K")))


;; test
(define (label-wire! wire label)
  ((wire 'label!) label))
(define (probe bus label)
  (let ([w (bus-width bus)])
    (for-each
     (lambda (n)
       (define (show)
         (for-each
          display
          (list label ": "))
         (bus-data bus)
         (newline))
       (((bus n) 'add-action!) show))
     (enum w))))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))

(define A (make-bus 16))
(define B (make-bus 16))
(define C (make-bus 16))
(define D (make-bus 16))
(define ADR (make-bus 10))

(probe A 'A)
(probe B 'B)
(probe C 'C)
(probe D 'D)

(define ld (make-wire))
(define inc (make-wire))
(define rst (make-wire))

(RAM1K A ld ADR B)
