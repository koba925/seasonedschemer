#lang racket/base

(require rackunit)
(require "util.rkt")

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond ((null? set) (quote ()))
                    ((member? (car set) set2)
                     (cons (car set) (I (cdr set))))
                    (else (I (cdr set)))))))
      (I set1))))

(check-equal? (intersect '() '()) '())
(check-equal? (intersect '(a) '()) '())
(check-equal? (intersect '() '(a)) '())
(check-equal? (intersect '(a) '(b)) '())
(check-equal? (intersect '(a) '(a)) '(a))
