#lang racket

(require sicp)
(require racket/trace)

(define (f-1 n)
  (cond
    ((< n 3) n)
    (else (+ (f-1 (- n 1)) (* 2 (f-1 (- n 2))) (* 3 (f-1 (- n 3)))))
  )
)
(f-1 3)

(define (f-2 n)
  (define (f-2-recursive nc f1 f2 f3)
    (define (calc a b c) (+ a (* 2 b) (* 3 c)))
    (cond
      ((= nc n) (calc f1 f2 f3))
      ((< nc 3) (f-2-recursive (+ nc 1) nc f1 f2))
      (else (f-2-recursive (+ nc 1) (calc f1 f2 f3) f1 f2))
    )
  )
  (f-2-recursive 0 0 -1 -2)
)

(f-2 3)
