#lang racket

(define deep
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (cons (deep (sub1 m)) (quote ())))))

(deep 0)
(deep 3)
