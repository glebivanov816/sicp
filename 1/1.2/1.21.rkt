#lang racket

(require sicp)
(require racket/trace)

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (divides? a b) (= (remainder b a) 0))
    (cond
      ((> (* test-divisor test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (+ test-divisor 1)))
    )
  )
  (find-divisor n 2)
)

(map smallest-divisor '(199 1999 19999))
