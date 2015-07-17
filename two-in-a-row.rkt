#lang racket/base

(require rackunit)
(require "util.rkt")

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (or (is-first? (car lat) (cdr lat))
                    (two-in-a-row? (cdr lat)))))))

(define is-first?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (eq? (car lat) a)))))

(check-false (two-in-a-row? '()))
(check-false (two-in-a-row? '(a)))
(check-true (two-in-a-row? '(a a)))
(check-true (two-in-a-row? '(a a b)))
(check-true (two-in-a-row? '(a b b)))
(check-false (two-in-a-row? '(a b c)))

