#lang racket

(require sicp)
(require racket/trace)

(define (simpsons-integral f a b n)
  (let ([h (/ (- b a) n)])
    (define (coef k)
      (cond
        ((or (= k a) (= k b)) 1)
        ((even? k) 4)
        (else 2)
      )
    )
    (define (sum k acc)
      (cond
        ((> k n) acc)
        (else (sum (+ k 1) (+ (* (coef k) (f (+ a (* k h)))) acc)))
      )
    )
    (* (/ h 3) (sum a 0))
  )
)

(define (cube x) (* x x x))

(simpsons-integral cube 0.0 1.0 100)
(simpsons-integral cube 0.0 1.0 1000)
(simpsons-integral cube 0.0 1.0 100000)
