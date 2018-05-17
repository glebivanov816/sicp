#lang racket

(require sicp)
(require racket/trace)
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess steps)
    (newline)
    (display guess)
    (newline)
    (display steps)
    (let ((next (f guess)))
      (if (close-enough? guess next) next (try next (+ 1 steps)))
    )
  )
  (try first-guess 1)
)

; (x ** x) = 1000
; log x (x ** x) = log (x ** x) / log (x)
; x = log 1000 / log (x)

(display "Without average damping")
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)

(display "With average damping")
(fixed-point (lambda (x) (+ (/ x 2) (/ (log 1000) (* 2 (log x))))) (+ 1 tolerance))
