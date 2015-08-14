#lang racket/base

(require rackunit)
(require "util.rkt")

(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))

(check-equal? (leftmost '(a)) 'a)
(check-equal? (leftmost '(a b)) 'a)
(check-equal? (leftmost '((a) b)) 'a)

(define leftmost2
  (lambda (l)
    (cond ((null? l) (quote ()))
          ((atom? (car l)) (car l))
          (else (cond
                  ((atom? (leftmost2 (car l)))
                   (leftmost2 (car l)))
                  (else (leftmost2 (cdr l))))))))

(check-equal? (leftmost2 '()) '())
(check-equal? (leftmost2 '(a)) 'a)
(check-equal? (leftmost2 '(a b)) 'a)
(check-equal? (leftmost2 '((a) b)) 'a)
(check-equal? (leftmost2 '(() (a) b)) 'a)
(check-equal? (leftmost2 '((()) (a) b)) 'a)
(check-equal? (leftmost2 '((()) (() a) b)) 'a)

(define leftmost3
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost3 (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost3 (cdr l)))))))))

(check-equal? (leftmost3 '()) '())
(check-equal? (leftmost3 '(a)) 'a)
(check-equal? (leftmost3 '(a b)) 'a)
(check-equal? (leftmost3 '((a) b)) 'a)
(check-equal? (leftmost3 '(() (a) b)) 'a)
(check-equal? (leftmost3 '((()) (a) b)) 'a)
(check-equal? (leftmost3 '((()) (() a) b)) 'a)

(define rember1*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (cdr l))
         (else (cons (car l) (rember1* a (cdr l))))))
      (else
       (cond
         ((eqlist? (rember1* a (car l)) (car l))
          (cons (car l) (rember1* a (cdr l))))
         (else
          (cons (rember1* a (car l)) (cdr l))))))))

(check-equal? (rember1* 'a '()) '())
(check-equal? (rember1* 'a '(a)) '())
(check-equal? (rember1* 'a '(b)) '(b))
(check-equal? (rember1* 'a '((a) b a)) '(() b a))
(check-equal? (rember1* 'a '((b) b a)) '((b) b))

(define rember1*2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (cond
                   ((eqlist? (R (car l)) (car l))
                    (cons (car l) (R (cdr l))))
                   (else
                    (cons (R (car l)) (cdr l)))))))))
      (R l))))

(check-equal? (rember1*2 'a '()) '())
(check-equal? (rember1*2 'a '(a)) '())
(check-equal? (rember1*2 'a '(b)) '(b))
(check-equal? (rember1*2 'a '((a) b a)) '(() b a))
(check-equal? (rember1*2 'a '((b) b a)) '((b) b))

(define rember1*3
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (let ((av (R (car l))))
                   (cond
                     ((eqlist? av (car l))
                      (cons (car l) (R (cdr l))))
                     (else
                      (cons av (cdr l))))))))))
      (R l))))

(check-equal? (rember1*3 'a '()) '())
(check-equal? (rember1*3 'a '(a)) '())
(check-equal? (rember1*3 'a '(b)) '(b))
(check-equal? (rember1*3 'a '((a) b a)) '(() b a))
(check-equal? (rember1*3 'a '((b) b a)) '((b) b))
