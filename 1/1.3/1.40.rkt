#lang racket

(require sicp)
(require racket/trace)

(define (fixed-point f guess)
  (define (close-enough? next guess) (< (abs (- next guess)) 0.000001))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? next guess) next (try next))
    )
  )
  (try guess)
)

(define dx 0.00001)
(define (derivative f) (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((derivative f) x))))
)

(define (newton-method f guess)
  (fixed-point (newton-transform f) guess)
)

(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(newton-method (cubic 1 1 2) 0.000001)
((cubic 1 1 2) -1.3532099641971034)
