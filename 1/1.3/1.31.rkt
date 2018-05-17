#lang racket

(require sicp)
(require racket/trace)

(define (identity x) x)

(define (product-i a b f next)
  (define (product-ii acc curr)
    (cond
      ((> curr b) acc)
      (else (product-ii (* acc (f curr)) (next curr)))
    )
  )
  (product-ii 1 a)
)

(define (product-r a b f next)
  (define (product-rr curr)
    (cond
      ((> curr b) 1)
      (else (* (f curr) (product-rr (next curr))))
    )
  )
  (product-rr a)
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
