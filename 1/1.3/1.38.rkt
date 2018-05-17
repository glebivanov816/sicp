#lang racket

(require sicp)
(require racket/trace)

(define (cont-frac-i n d k)
  (define (cont-frac-int j m)
    (cond
      ((= j 0) m)
      (else (cont-frac-int (- j 1) (/ (n j) (+ m (d j)))))
    )
  )
  (cont-frac-int k (d 0))
)

(define (exponent)
  (define (next-d i) (if (= (/ (+ 1 i) 3) 0) (* (/ (+ 1 i) 3) 2) 1))
  (+ 2 (cont-frac-i (lambda (i) 1.0) next-d 10))
)

(exponent)
