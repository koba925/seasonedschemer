#lang racket/base

(require rackunit)

(provide atom? add1 sub1 one? eqlist? pick member? Y)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(check-true (atom? 0))
(check-true (atom? (quote a)))
(check-false (atom? (quote ())))
(check-false (atom? (quote (a b))))

(define add1
  (lambda (n) (+ n 1)))

(check-equal? (add1 1) 2)

(define sub1
  (lambda (n) (- n 1)))

(check-equal? (sub1 2) 1)

(define one?
  (lambda (n) (zero? (sub1 n))))

(check-true (zero? 0))
(check-false (zero? 1))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(check-true (eqlist? '() '()))
(check-false (eqlist? '(a b) '(a)))
(check-false (eqlist? '(b) '(a b)))
(check-false (eqlist? '((a) b) '(a b)))
(check-false (eqlist? '(a (b)) '(a b)))
(check-true (eqlist? '((a) (b)) '((a) (b))))

(define pick
  (lambda (n lat)
    (cond ((one? n) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(check-equal? (pick 1 '(2)) 2)
(check-equal? (pick 2 '(1 2 3)) 2)

(define member?
  (lambda (a lat)
    (letrec
        ((M (lambda (lat)
              (cond ((null? lat) #f)
                    (else (or (eq? (car lat) a)
                              (M (cdr lat))))))))
      (M lat))))

(check-false (member? 'a '()))
(check-true (member? 'a '(a)))
(check-false (member? 'a '(b)))
(check-true (member? 'a '(b a)))
(check-false (member? 'a '(b b)))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f) (le (lambda (x) ((f f) x)))))))

(check-equal?
 ((Y
   (lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l))))))))
  '(a b c))
 3)
