#lang racket

(require racket/trace)

(define (exponent num pow)
  (define (even x) (= 0 (bitwise-and x 1)))
  (define (recursive-exponent acc current-pow)
    (cond
      ((= 0 current-pow) acc)
      ((even current-pow) (* acc num num (recursive-exponent acc (- current-pow 2))))
      (else (* acc num (recursive-exponent acc (- current-pow 1))))
    )
  )
  (recursive-exponent 1 pow)
)

(exponent 2 3)
