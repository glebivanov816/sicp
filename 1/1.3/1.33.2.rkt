#lang racket

(require sicp)
(require racket/trace)

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

(define (product-of-rel-primes a b)
  (define (identity n) n)
  (define (gcd a b) (if (= b 0) a (gcd b (modulo a b))))
  (define (rel-prime? n) (= (gcd b n) 1))
  (accumulate-i a b 1 identity * rel-prime? (lambda (x) (+ x 1)))
)

(product-of-rel-primes 1 5)
