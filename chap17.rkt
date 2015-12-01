#lang racket

(require "util.rkt")

(define find
  (lambda (n Ns Rs)
    (cond ((null? Ns) #f)
          ((eq? (car Ns) n) (car Rs))
          (else (find n (cdr Ns) (cdr Rs))))))

(find 3 '(1 2 3 4) '(a b c d))
(find 0 '(1 2 3 4) '(a b c d))

(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       (quote pizza)
                       (cons (deepM (sub1 n)) (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(deepM 0)
(deepM 3)

(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deep
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (consC (deep (sub1 m)) (quote ())))))

(deep 0)
(deep 3)

