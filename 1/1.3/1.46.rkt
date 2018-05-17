#lang racket

(require sicp)
(require racket/trace)

(define (iterative-improve tester improver)
  (lambda (initial)
    (define (try guess)
      (let ([next (improver guess)])
        (if (tester guess next) next (try next))
      )
    )
    (try initial)
  )
)

(define (square-root number)
  (define (close-enough? guess next) (< (abs (- guess next)) 0.001))
  (define (improver guess) (/ (+ guess (/ number guess)) 2))
  ((iterative-improve close-enough? improver) number)
)

(define (fixed-point f guess)
  (define (close-enough? guess next) (< (abs (- next guess)) 0.000001))
  ((iterative-improve close-enough? f) guess)
)

(define (fp-square-root x)
  (define (average-damp f) (lambda (x) (/ (+ x (f x)) 2)))
  (fixed-point (average-damp (lambda (y) (/ x y))) 0.5)
)

(square-root 16.0)
(fp-square-root 16.0)
