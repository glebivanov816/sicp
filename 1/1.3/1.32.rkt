#lang racket

(require sicp)
(require racket/trace)

(define (identity x) x)

(define (accumulate-i a b c f g next)
  (define (accumulate-ii acc curr)
    (cond
      ((> curr b) acc)
      (else (accumulate-ii (g acc (f curr)) (next curr)))
    )
  )
  (accumulate-ii c a)
)

(define (accumulate-r a b c f g next)
  (define (accumulate-rr curr)
    (cond
      ((> curr b) c)
      (else (g (f curr) (accumulate-rr (next curr))))
    )
  )
  (accumulate-rr a)
)

(define (product-i a b f next)
  (accumulate-i a b 1 f * next)
)

(define (product-r a b f next)
  (accumulate-r a b 1 f * next)
)

(define (factorial n)
  (product-r 1 n identity (lambda (b) (+ b 1)))
)

(define (quarter-pi n)
  (define (fn k)
    (cond
      ((even? k) (/ (+ k 2) (+ k 1)))
      ((odd? k) (/ (+ k 1) (+ k 2)))
    )
  )
  (product-i 1.0 n fn (lambda (x) (+ 1 x)))
)

(product-i 1 3 identity (lambda (x) (+ x 1)))
(product-r 1 3 identity (lambda (x) (+ x 1)))

(factorial 4)

(quarter-pi 10000)
(/ pi 4)
