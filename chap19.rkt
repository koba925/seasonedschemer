#lang racket

(require "util.rkt")

(define deep
  (lambda (m)
    (cond ((zero? m) (quote pizza))
          (else (cons (deep (sub1 m)) (quote ()))))))

(newline)
(deep 6)

(define six-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons
        (cons
         (cons p (quote ()))
         (quote ()))
        (quote ()))
       (quote ()))
      (quote ()))
     (quote ()))))

(newline)
(six-layers (quote pizza))
(six-layers (quote mozzarella))

(define four-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons p (quote ()))
       (quote ()))
      (quote ()))
     (quote ()))))

(newline)
(deep 4)
(four-layers (quote pizza))

(define deepN
  (lambda (m p)
    (cond ((zero? m) p)
          (else (cons (deepN (sub1 m) p) (quote ()))))))

(newline)
(deepN 4 (quote pizza))
(deepN 4 (quote mozzarella))

(define toppings #f)
(define deepB
  (lambda (m)
    (cond ((zero? m)
           (let/cc jump
             (set! toppings jump)
             (quote pizza)))
          (else (cons (deepB (sub1 m)) (quote ()))))))

(newline)
(deepB 6)
(toppings (quote mozzarella))
(toppings (quote cake))
(toppings (quote pizza))

(newline)
(cons (toppings (quote cake)) (quote ()))
(cons
 (cons
  (cons (toppings (quote mozzarella)) (quote ()))
  (quote()))
 (quote()))

(newline)
(deepB 4)
(cons
 (cons
  (cons (toppings (quote mozzarella)) (quote ()))
  (quote()))
 (quote()))

(newline)
(cons (toppings (quote cake))
      (toppings (quote cake)))
(cons (toppings (quote cake))
      (cons (toppings (quote mozzarella))
            (cons (toppings (quote pizza))
                  (quote ()))))

(define deep&co
  (lambda (m k)
    (cond ((zero? m) (k (quote pizza)))
          (else (deep&co (sub1 m) (lambda (x) (k (cons x (quote ())))))))))

(newline)
(deep&co 0 (lambda (x) x))
(deep&co 6 (lambda (x) x))
(deep&co 2 (lambda (x) x))

(define deep&coB
  (lambda (m k)
    (cond ((zero? m)
           (let ()
             (set! toppings k)
             (k (quote pizza))))
          (else
           (deep&coB (sub1 m)
                     (lambda (x) (k (cons x (quote ())))))))))

(newline)
(deep&coB 2 (lambda (x) x))
(toppings (quote pizza))
(deep&coB 6 (lambda (x) x))
(toppings (quote pizza))
(deep&coB 4 (lambda (x) x))
(toppings (quote pizza))

(cons (toppings (quote cake))
      (toppings (quote cake)))
(cons (toppings (quote cake))
      (cons (toppings (quote mozzarella))
            (cons (toppings (quote pizza))
                  (quote ()))))