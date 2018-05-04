#lang racket

(require sicp)
(require racket/trace)

(define (square-root number) (recursive-square-root number 1 0))

(define (recursive-square-root number guess last-guess)
  (cond
    ((is-square-root-enough? guess last-guess) guess)
    (else (recursive-square-root number (new-square-root-guess number guess) guess))
  )
)

(define (is-square-root-enough? guess last-guess) (< (abs (- last-guess guess)) 0.001))
(define (new-square-root-guess number guess) (/ (+ guess (/ number guess)) 2))

(exact->inexact (square-root 16))
