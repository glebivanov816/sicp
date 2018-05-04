#lang racket

(require sicp)
(require racket/trace)

(define (sum-of-squares-for-largest-of a b c)
  (cond
    ((and (< a b) (< a c)) (sum-of-squares b c))
    ((and (< b a) (< b c)) (sum-of-squares a c))
    (else (sum-of-squares a b))
  )
)

(define (sum-of-squares a b) (+ (* a a) (* b b)))

(sum-of-squares-for-largest-of 1 2 3)
