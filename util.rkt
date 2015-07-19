#lang racket/base

(require rackunit)

(provide atom? add1 sub1 one? pick)

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

(define one?
  (lambda (n) (zero? (sub1 n))))

(check-true (zero? 0))
(check-false (zero? 1))

(define pick
  (lambda (n lat)
    (cond ((one? n) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(check-equal? (pick 1 '(2)) 2)
(check-equal? (pick 2 '(1 2 3)) 2)
