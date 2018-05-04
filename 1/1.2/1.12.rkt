#lang racket

(require racket/trace)

(define (pascal-triangle depth)
  (define (recursive-pascal-triangle level prev-list)
    (define (build-current-list)
      (build-list level (lambda (x)
      (cond
        ((or (= x 0) (= (- level 1) x)) 1)
        (else (+ (list-ref prev-list (- x 1)) (list-ref prev-list x)))
      )
    )))

    (let ([current-list (build-current-list)])
      (cond
        ((> level depth) (list))
        (else (list* current-list (recursive-pascal-triangle (+ level 1) current-list)))
      )
    )
  )
  (recursive-pascal-triangle 1 (list))
)

(pascal-triangle 10)
