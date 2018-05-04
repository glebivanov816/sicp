#lang racket

(require sicp)
(require racket/trace)

(define (timed-prime-test n start-time)
  (let ([is-prime (miller-rabin-test n)])
    (if is-prime (report-prime n (- (runtime) start-time)))
    is-prime
  )
)

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
)

(define (miller-rabin-test n)
  (define (try-it a) (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1))))
)

(define (expmod base exp m)
  (define (square x) (* x x))
  (define (square-check x m)
    (if (and
          (not (or (= x 1) (= x (- m 1))))
          (= (remainder (* x x) m) 1)
        )
        0
        (remainder (* x x) m)
    )
  )
  (cond
    ((= base 0) 0)
    ((= exp 0) 1)
    ((even? exp) (square-check (expmod base (/ exp 2) m) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))
  )
)

(map (lambda (x) (timed-prime-test x (runtime))) '(561 1105 1729 2465 2821 6601))
