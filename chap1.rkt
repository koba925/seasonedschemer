#lang racket/base

(require rackunit)
(require "util.rkt")

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (two-in-a-row-b? (car lat) (cdr lat))))))

(define two-in-a-row-b?
  (lambda (preceeding lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) preceeding)
                    (two-in-a-row-b? (car lat) (cdr lat)))))))

(check-false (two-in-a-row? '()))
(check-false (two-in-a-row? '(a)))
(check-true (two-in-a-row? '(a a)))
(check-true (two-in-a-row? '(a a b)))
(check-true (two-in-a-row? '(a b b)))
(check-false (two-in-a-row? '(a b c)))

(define sum-of-prefixes
  (lambda (tup)
    (cond ((null? tup) (quote ()))
          (else (sum-of-prefixes-b 0 tup)))))

;+はあることにする
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond ((null? tup) (quote ()))
          (else (cons (+ sonssf (car tup))
                      (sum-of-prefixes-b (+ sonssf (car tup))
                                         (cdr tup)))))))

(check-equal? (sum-of-prefixes '()) '())
(check-equal? (sum-of-prefixes '(1)) '(1))
(check-equal? (sum-of-prefixes '(1 2 3)) '(1 3 6))