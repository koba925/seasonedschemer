#lang racket

(require "util.rkt")

(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

(define kar
  (lambda (c)
    (c (lambda (a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (a d) d))))

(newline)

(kons 2 (kons 1 (quote ())))

(kar (kons 2 (kons 1 (quote ()))))
(kar (kdr (kons 2 (kons 1 (quote ())))))
(kdr (kdr (kons 2 (kons 1 (quote ())))))
