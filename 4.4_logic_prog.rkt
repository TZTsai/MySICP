#lang racket/base

(require "sicp-lang.rkt")

;; Ex 4.55
; 1. (supervisor ?p (Bitdiddle Ben))
; 2. (job ?p (accounting . ?type))
; 3. (address ?p (Slumerville . ?addr))

;; Ex 4.56
; 1. (and (supervisor ?p (Bitdiddle Ben))
;         (address ?p ?a))
; 2. (and (salary ?p ?s1)
;         (salary (Bitdiddle Ben) ?s2)
;         (< s1 s2))
; 3. (and (supervisor ?x ?y)
;         (job ?y (?div . ?type))
;         (different ?div computer))

;; Ex 4.57
; 1. (rule ...



