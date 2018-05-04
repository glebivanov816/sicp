#lang racket

(require sicp)
(require racket/trace)

(define (timed-prime-test n start-time)
  (let ([is-prime (fast-prime? n 1)])
    (if is-prime (report-prime n (- (runtime) start-time)))
    is-prime
  )
)

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
)

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)
  )
)

(define (fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1))))
)

(define (expmod base exp m)
  (define (square x) (* x x))
  (cond
    ((= exp 0) 1)
    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))
  )
)

(map (lambda (x) (timed-prime-test x (runtime))) '(561 1105 1729 2465 2821 6601))
