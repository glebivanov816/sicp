#lang racket

(require sicp)
(require racket/trace)

(define (cont-frac n d k)
  (define (cont-frac-int j)
    (cond
      ((> j k) 0)
      (else (/ (n j) (+ (d j) (cont-frac-int (+ j 1)))))
    )
  )
  (cont-frac-int 0)
)

(define (cont-frac-i n d k)
  (define (cont-frac-int j m)
    (cond
      ((= j 0) m)
      (else (cont-frac-int (- j 1) (/ (n j) (+ m (d j)))))
    )
  )
  (cont-frac-int k (d 0))
)

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
(cont-frac-i (lambda (i) 1.0) (lambda (i) 1.0) 10)

(define (golden-ratio) (/ (+ 1 (sqrt 5)) 2))
(/ 1.0 (golden-ratio))
