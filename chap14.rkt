#lang racket/base

(require rackunit)
(require "util.rkt")

(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))

(check-equal? (leftmost '(a)) 'a)
(check-equal? (leftmost '(a b)) 'a)
(check-equal? (leftmost '((a) b)) 'a)
