#lang racket/base

(require rackunit)
(require "util.rkt")

(define multirember
  (lambda (a lat)
    ((Y
      (lambda (mr)
        (lambda (lat)
          (cond ((null? lat) (quote ()))
                ((eq? (car lat) a) (mr (cdr lat)))
                (else (cons (car lat) (mr (cdr lat))))))))
     lat)))

(check-equal? (multirember 'a '()) '())
(check-equal? (multirember 'a '(a b)) '(b))
(check-equal? (multirember 'a '(b c)) '(b c))
(check-equal? (multirember 'a '(a b a)) '(b))

