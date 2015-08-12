#lang racket/base

(require rackunit)
(require "util.rkt")

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond ((null? set) (quote ()))
                    ((member? (car set) set2)
                     (cons (car set) (I (cdr set))))
                    (else (I (cdr set)))))))
      (cond ((null? set2) (quote ()))
            (else (I set1))))))

(check-equal? (intersect '() '()) '())
(check-equal? (intersect '(a) '()) '())
(check-equal? (intersect '() '(a)) '())
(check-equal? (intersect '(a) '(b)) '())
(check-equal? (intersect '(a) '(a)) '(a))

(define intersectall
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond ((null? (car lset))
                       (hop (quote ())))
                      ((null? (cdr lset)) (car lset))
                      (else (I (car lset) (A (cdr lset)))))))
           (I (lambda (set1 set2)
                (letrec
                    ((J (lambda (set1)
                          (cond ((null? set1) (quote ()))
                                ((member? (car set1) set2)
                                 (cons (car set1) (J (cdr set1))))
                                (else (J (cdr set1)))))))
                  (cond ((null? set2) (hop (quote ())))
                        (else (J set1)))))))
        (cond ((null? lset) (quote()))
              (else (A lset)))))))

(check-equal? (intersectall '()) '())
(check-equal? (intersectall '(())) '())
(check-equal? (intersectall '((a))) '(a))
(check-equal? (intersectall '((a) (a))) '(a))
(check-equal? (intersectall '((a) (b))) '())
(check-equal? (intersectall '((a) (a) (a))) '(a))
(check-equal? (intersectall '((a) (a) (b))) '())
(check-equal? (intersectall '((a b c) (c a d) (b e c))) '(c))
(check-equal? (intersectall '((a b c) () (b e c))) '())

; (intersectall-c '((a b c) () (b e c)))
; (let/cc hop (A '((a b c) () (b e c))))
; (let/cc hop (intersect '(a b c) (A '(() (b e c)))))
; (let/cc hop (intersect '(a b c) (hop (quote ()))))
; (let/cc hop (quote ()))
; (quote ())



