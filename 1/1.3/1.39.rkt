#lang racket

(require sicp)
(require racket/trace)

(define (cont-frac n d k)
  (define (cont-frac-int j)
    (cond
      ((> j k) 0)
      (else (/ (n j) (- (d j) (cont-frac-int (+ j 1)))))
    )
  )
  (cont-frac-int 1)
)

(define (tan-cf x k)
  (define (next-n i) (if (= i 1) x (* x x)))
  (define (next-d i) (+ i (- i 1)))
  (cont-frac next-n next-d k)
)

(tan-cf (/ pi 4) 1000)
