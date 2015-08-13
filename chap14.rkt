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

(define leftmost2
  (lambda (l)
    (cond ((null? l) (quote ()))
          ((atom? (car l)) (car l))
          (else (cond
                  ((atom? (leftmost2 (car l)))
                   (leftmost2 (car l)))
                  (else (leftmost2 (cdr l))))))))

(check-equal? (leftmost2 '()) '())
(check-equal? (leftmost2 '(a)) 'a)
(check-equal? (leftmost2 '(a b)) 'a)
(check-equal? (leftmost2 '((a) b)) 'a)
(check-equal? (leftmost2 '(() (a) b)) 'a)
(check-equal? (leftmost2 '((()) (a) b)) 'a)
(check-equal? (leftmost2 '((()) (() a) b)) 'a)
