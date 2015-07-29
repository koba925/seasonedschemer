#lang racket/base

(require rackunit)
(require "util.rkt")

(define multirember-y
  (lambda (a lat)
    ((Y
      (lambda (mr)
        (lambda (lat)
          (cond ((null? lat) (quote ()))
                ((eq? (car lat) a) (mr (cdr lat)))
                (else (cons (car lat) (mr (cdr lat))))))))
     lat)))

(check-equal? (multirember-y 'a '()) '())
(check-equal? (multirember-y 'a '(a b)) '(b))
(check-equal? (multirember-y 'a '(b c)) '(b c))
(check-equal? (multirember-y 'a '(a b a)) '(b))

(define multirember-r
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond ((null? lat) (quote ()))
                      ((eq? (car lat) a) (mr (cdr lat)))
                      (else (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))

(check-equal? (multirember-r 'a '()) '())
(check-equal? (multirember-r 'a '(a b)) '(b))
(check-equal? (multirember-r 'a '(b c)) '(b c))
(check-equal? (multirember-r 'a '(a b a)) '(b))


(define multirember-d
  (lambda (a lat)
    (define mr
      (lambda (lat)
        (cond ((null? lat) (quote ()))
              ((eq? a (car lat)) (mr (cdr lat)))
              (else (cons (car lat) (mr (cdr lat)))))))
    (mr lat)))

(check-equal? (multirember-d 'a '()) '())
(check-equal? (multirember-d 'a '(a b)) '(b))
(check-equal? (multirember-d 'a '(b c)) '(b c))
(check-equal? (multirember-d 'a '(a b a)) '(b))

(define multirember-r2
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond ((null? lat) (quote ()))
                     ((eq? (car lat) a) (mr (cdr lat)))
                     (else (cons (car lat) (mr (cdr lat))))))))
      (mr lat))))

(check-equal? (multirember-r2 'a '()) '())
(check-equal? (multirember-r2 'a '(a b)) '(b))
(check-equal? (multirember-r2 'a '(b c)) '(b c))
(check-equal? (multirember-r2 'a '(a b a)) '(b))