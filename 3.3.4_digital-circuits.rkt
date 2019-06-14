#lang racket
(require "sicp-lang.rkt" "3.3.2_queues.rkt")

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; primitive logic gates
(define (inverter input output)
  (define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))
  (make-logic-component (list input) output logical-not inverter-delay))

(define (and-gate a1 a2 output)
  (define (logical-and s1 s2)
    (cond [(and (= s1 1) (= s2 1)) 1]
          [(and (= s1 0) (= s2 1)) 0]
          [(and (= s1 1) (= s2 0)) 0]
          [(and (= s1 0) (= s2 0)) 0]
          [else (error "Invalid signal" s1 s2)]))
  (make-logic-component (list a1 a2) output logical-and and-gate-delay))

(define (or-gate s1 s2 out)
  (define (logical-or s1 s2)
    (cond [(and (= s1 1) (= s2 1)) 1]
          [(and (= s1 0) (= s2 1)) 1]
          [(and (= s1 1) (= s2 0)) 1]
          [(and (= s1 0) (= s2 0)) 0]
          [else (error "Invalid signal" s1 s2)]))
  (make-logic-component (list s1 s2) out logical-or or-gate-delay))

(define (make-logic-component wires-in wire-out logic delay)
  (define (action-procedure)
    (let ([output (apply logic (map get-signal wires-in))])
      (after-delay delay
                   (lambda ()
                     (set-signal! wire-out output)))))
  (for-each (lambda (wire-in)
              (add-action! wire-in action-procedure))
            wires-in)
  'ok)

;; imp a wire
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal)
             signal-value)
            ((eq? m 'set-signal!)
             set-my-signal!)
            ((eq? m 'add-action!)
             accept-action-procedure!)
                        (else (error "Unknown operation:
                          WIRE" m))))
        dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


;; agenda
(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; impl of agenda
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time
           (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue!
         (segment-queue (car segments))
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment
                      time
                      action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment
                time
                action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments!
         agenda
         (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
            (error "Agenda is empty:
              FIRST-AGENDA-ITEM")
            (let ((first-seg
                   (first-segment agenda)))
              (set-current-time!
               agenda
               (segment-time first-seg))
              (front-queue
               (segment-queue first-seg)))))

;; probe
(define (probe name wire)
  (add-action!
   wire
   (lambda ()
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire))
     (newline))))

;; test
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(half-adder input-1 input-2 sum carry)
(probe 'sum sum)
(probe 'carry carry)
