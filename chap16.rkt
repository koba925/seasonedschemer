#lang racket/base

(require rackunit)
(require "util.rkt")

(define sweet-tooth
  (lambda (food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(define last (quote angelfood))

(sweet-tooth (quote chocolate))
last

(sweet-tooth (quote fruit))
last

(newline)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(sweet-toothL (quote chocolate))
last

(sweet-toothL (quote fruit))
last

(sweet-toothL (quote cheese))
(sweet-toothL (quote carrot))

(newline)

(define ingredients (quote ()))
(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food
          (cons (quote cake)
                (quote ())))))

(sweet-toothR (quote chocolate))
ingredients
(sweet-toothR (quote fruit))
ingredients
(sweet-toothR (quote cheese))
ingredients
(sweet-toothR (quote carrot))
ingredients

(newline)

(define deep
  (lambda (m)
    (cond ((zero? m) (quote pizza))
          (else (cons (deep (sub1 m)) (quote ()))))))

(deep 3)
(deep 7)
(deep 0)

(newline)

(define Ns (quote ()))
(define Rs (quote ()))
(define deepR
  (lambda (n)
    (let ((result (deep n)))
    (set! Rs (cons result Rs))
    (set! Ns (cons n Ns))
    result)))

(deepR 3)
Ns
Rs

(deepR 5)
Ns
Rs

(deepR 3)
Ns
Rs

(newline)

;(define find
;  (lambda (n Ns Rs)
;    (cond ((null? Ns) (quote ()))
;          ((eq? (car Ns n)) (car Rs))
;          (else (find n (cdr Ns) (cdr Rs))))))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond ((= (car ns) n) (car rs))
                    (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(find 3 Ns Rs)
(find 5 Ns Rs)
;(find 7 Ns Rs)

(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (deepR n))))

(newline)

(set! Ns (cdr Ns))
(set! Rs (cdr Rs))
Ns
Rs

(deepM 3)
(deepM 5)
(deepM 7)
Ns
Rs

(newline)

(define deepM2
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))

(set! Ns (cdr Ns))
(set! Rs (cdr Rs))
Ns
Rs

(deepM2 6)
Ns
Rs

(newline)

(define deep3
  (lambda (m)
    (cond ((zero? m) (quote pizza))
          (else (cons (deepM3 (sub1 m)) (quote ()))))))

(define deepM3
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep3 n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))

(deepM3 3)
(deepM3 5)
(deepM3 7)
(deepM3 9)
Ns
Rs

(define deep4
  (lambda (m)
    (cond ((zero? m) (quote pizza))
          (else (cons (deepM4 (sub1 m)) (quote ()))))))

(define deepM4
  (let ((Ns (quote ()))
        (Rs (quote ())))
    (lambda (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((result (deep4 n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))

(newline)
(deepM4 3)
;(deepM4 16)
