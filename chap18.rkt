#lang racket

(require "util.rkt")

(define bons
  (lambda (kar)
    (let ((kdr (quote ())))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define set-kdr
  (lambda (c x) ((c (lambda (s a d) s)) x)))

(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

(bons 1)
(kar (bons 1))
(kdr (bons 1))
(set-kdr (bons 1) 2)
(let ((c (bons 1)))
  (set-kdr c 2)
  (kdr c))

(define adom?
  (lambda (s)
    (and (atom? s) (not (procedure? s)))))

(define wride
  (lambda (l)
    (letrec ((W (lambda (l)
                  (cond ((null? l) (display "'()"))
                        ((adom? l) (write l))
                        (else
                         (display "(kons ")
                         (W (kar l))
                         (display " ")
                         (W (kdr l))
                         (display ")"))))))
      (W l)
      (newline))))

(newline)
(wride (kons 1 (kons (kons 2 (kons 3 '())) '())))
(define l (kons 1 (kons 2 '())))
(wride l)
(set-kdr l (kons 2 (kons 3 '())))
(wride l)
(wride (kdr l))
(wride (kar (kdr l)))

(define gons
  (lambda (kar kdr)
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr))))

(newline)
(define m (gons 1 (gons 2 '())))
(wride m)
(set-kdr m (gons 2 (gons 3 '())))
(wride m)
(wride (kdr m))
(wride (kar (kdr m)))

