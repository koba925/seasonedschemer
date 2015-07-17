#lang racket/base

(require rackunit)
(require "util.rkt")

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (is-first-b? (car lat) (cdr lat)))))))

(check-false (two-in-a-row? '()))
(check-false (two-in-a-row? '(a)))
(check-true (two-in-a-row? '(a a)))
(check-true (two-in-a-row? '(a a b)))
(check-true (two-in-a-row? '(a b b)))
(check-false (two-in-a-row? '(a b c)))

