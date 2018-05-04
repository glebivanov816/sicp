#lang racket

(require sicp)
(require racket/trace)

(define (run-multiple-tests begin-with count-of-primes)
  (cond
    ((= 0 count-of-primes))
    ((even? begin-with) (run-multiple-tests (+ 1 begin-with) count-of-primes))
    ((timed-prime-test begin-with (runtime)) (run-multiple-tests (+ 2 begin-with) (- count-of-primes 1)))
    (else (run-multiple-tests (+ 1 begin-with) count-of-primes))
  )
)

(define (timed-prime-test n start-time)
  (let ([is-prime (prime? n)])
    (if is-prime (report-prime n (- (runtime) start-time)))
    is-prime
  )
)

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
)

(define (prime? n)
  (define (smallest-divisor)
    (define (find-divisor test-divisor)
      (define (divides? a b) (= (remainder b a) 0))
      (cond
        ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor (+ test-divisor 1)))
      )
    )
    (find-divisor 2)
  )
  (= n (smallest-divisor))
)

(run-multiple-tests 1000 3)
(run-multiple-tests 10000 3)
(run-multiple-tests 100000 3)
(run-multiple-tests 1000000 3)
