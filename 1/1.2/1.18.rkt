#lang racket

(define (mult num times)
  (define (even x) (= 0 (bitwise-and x 1)))
    (define (recursive-mult acc current-times)
      (cond
        ((= 0 current-times) acc)
        ((even current-times) (+ acc num num (recursive-mult acc (- current-times 2))))
        (else (+ acc num (recursive-mult acc (- current-times 1))))
      )
    )
    (recursive-mult 0 times)
)

(mult 2 1)
