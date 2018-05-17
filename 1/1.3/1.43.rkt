#lang racket

(require sicp)
(require racket/trace)

(define (square x) (* x x))
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n) (if (> n 1) (compose f (repeated f (- n 1))) f))

((repeated square 2) 5)
