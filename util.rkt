#lang racket/base

(require rackunit)

(provide atom? add1 sub1)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(check-true (atom? 0))
(check-true (atom? (quote a)))
(check-false (atom? (quote ())))
(check-false (atom? (quote (a b))))

(define add1
  (lambda (n) (+ n 1)))

(check-equal? (add1 1) 2)

(define sub1
  (lambda (n) (- n 1)))

(check-equal? (sub1 2) 1)

