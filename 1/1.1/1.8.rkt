#lang racket

(require sicp)
(require racket/trace)

(define (cube-root number)
  (define (recursive-root guess last-guess)
    (cond
      ((is-good-enough? guess last-guess) guess)
      (else (recursive-root (new-root-guess guess) guess))
    )
  )

  (define (is-good-enough? guess last-guess) (< (abs (- last-guess guess)) 0.001))
  (define (new-root-guess guess) (/ (+ (* guess 2) (/ number (* guess guess))) 3))

  (recursive-root 1 0)
)

(exact->inexact (cube-root 8))
