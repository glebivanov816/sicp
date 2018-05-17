#lang racket

(require sicp)
(require racket/trace)

(define (double f) (lambda (x) (f (f x))))

; x -> double double x
; x -> double double double double x // 2 ** 4 inc = 16 inc

(((double (double double)) inc) 5)
