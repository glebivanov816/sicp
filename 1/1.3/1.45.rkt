#lang racket

(require sicp)
(require racket/trace)

(define dx 0.0001)
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n) (if (> n 1) (compose f (repeated f (- n 1))) f))

(define (fixed-point f guess)
  (define (close-enough? next guess) (< (abs (- next guess)) 0.000001))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? next guess) next (try next))
    )
  )
  (try guess)
)

(define (average-damp f) (lambda (x) (/ (+ x (f x)) 2)))

(define (root x power)
  (define (f y) (/ x (expt y (- power 1))))
  (fixed-point ((repeated average-damp (- power 1)) f) 0.5)
)

(root 64 6)
