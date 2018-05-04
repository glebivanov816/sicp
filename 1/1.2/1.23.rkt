#lang racket

(require sicp)
(require racket/trace)

(define (timed-prime-test n start-time)
  (let ([is-prime (prime? n)])
    (if is-prime (report-prime n (- (runtime) start-time)))
    is-prime
  )
)

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
)

(define (prime? n)
  (define (smallest-divisor)
    (define (find-divisor test-divisor)
      (define (divides? a b) (= (remainder b a) 0))
      (define (next-test-divisor) (if (= test-divisor 2) 3 (+ 2 test-divisor)))
      (cond
        ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor (next-test-divisor)))
      )
    )
    (find-divisor 2)
  )
  (= n (smallest-divisor))
)

(map (lambda (x) (timed-prime-test x (runtime))) '(1009 1013 1019 10007 10009 10037 100003 100019 100043))
