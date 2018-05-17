#lang racket

(require sicp)
(require racket/trace)

(define (prime? n)
  (define (r) (if (< n 2) n (- n 1)))
  (define (try-it a) (= (expmod a n n) a))
  (try-it (random (r)))
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

(define (accumulate-i a b c f g p next)
  (define (accumulate-ii acc curr)
    (cond
      ((> curr b) acc)
      ((p curr) (accumulate-ii (g acc (f curr)) (next curr)))
      (else (accumulate-ii acc (next curr)))
    )
  )
  (accumulate-ii c a)
)

(define (sum-of-squares-of-primes a b)
  (define (square n) (* n n))
  (accumulate-i a b 0 square + prime? (lambda (x) (+ x 1)))
)

(sum-of-squares-of-primes 1 5)
