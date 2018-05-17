#lang racket

(require sicp)
(require racket/trace)

(define dx 0.0001)
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n) (if (> n 1) (compose f (repeated f (- n 1))) f))
(define (smooth f) (lambda (x) (/ (f (- x dx)) (f x) (f (+ x dx)) 3)))
(define (n-smooth f n) ((repeated smooth n) f))

((n-smooth inc 10) 1)
