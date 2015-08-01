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

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) (quote ()))
            ((test? (car l) a) (cdr l))
            (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(check-equal? (rember-eq? 'a '()) '())
(check-equal? (rember-eq? 'a '(a)) '())
(check-equal? (rember-eq? 'a '(b)) '(b))
(check-equal? (rember-eq? 'a '(a b)) '(b))
(check-equal? (rember-eq? 'a '(b a)) '(b))
(check-equal? (rember-eq? 'a '((a) b)) '((a) b))

(define rember-equal? (rember-f equal?))

(check-equal? (rember-equal? '(a) '((a) b)) '(b))

(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) (quote ()))
            ((test? (car l) a) ((multirember-f test?) a (cdr l)))
            (else (cons (car l) ((multirember-f test?) a (cdr l))))))))

(define multirember-eq? (multirember-f eq?))

(check-equal? (multirember-eq? 'a '(a b)) '(b))
(check-equal? (multirember-eq? 'a '(b c)) '(b c))
(check-equal? (multirember-eq? 'a '(a b a)) '(b))

(define multirember-equal? (multirember-f equal?))

(check-equal? (multirember-equal? '(a) '((a) b)) '(b))
(check-equal? (multirember-equal? '(a) '(b c)) '(b c))
(check-equal? (multirember-equal? '(a) '((a) b (a))) '(b))

(define multirember-f2
  (lambda (test?)
    (letrec
        ((m-f
          (lambda (a l)
            (cond ((null? l) (quote ()))
                  ((test? (car l) a) (m-f a (cdr l)))
                  (else (cons (car l) (m-f a (cdr l))))))))
      m-f)))

(define multirember-eq?2 (multirember-f2 eq?))

(check-equal? (multirember-eq?2 'a '(a b)) '(b))
(check-equal? (multirember-eq?2 'a '(b c)) '(b c))
(check-equal? (multirember-eq?2 'a '(a b a)) '(b))

(define multirember-equal?2 (multirember-f2 equal?))

(check-equal? (multirember-equal?2 '(a) '((a) b)) '(b))
(check-equal? (multirember-equal?2 '(a) '(b c)) '(b c))
(check-equal? (multirember-equal?2 '(a) '((a) b (a))) '(b))

;第3のmultirember
;aを引数に取ってる
;なぜここで
(define multirember-r3
  (letrec
      ((mr (lambda (a lat)
             (cond ((null? lat) (quote ()))
                   ((eq? (car lat) a) (mr a (cdr lat)))
                   (else (cons (car lat) (mr a (cdr lat))))))))
    mr))

(check-equal? (multirember-r3 'a '()) '())
(check-equal? (multirember-r3 'a '(a b)) '(b))
(check-equal? (multirember-r3 'a '(b c)) '(b c))
(check-equal? (multirember-r3 'a '(a b a)) '(b))

;何をやっているんだ
(define multirember-r4
  (letrec
      ((multirember
        (lambda (a lat)
          (cond ((null? lat) (quote ()))
                ((eq? (car lat) a) (multirember a (cdr lat)))
                (else (cons (car lat) (multirember a (cdr lat))))))))
    multirember))

(check-equal? (multirember-r4 'a '()) '())
(check-equal? (multirember-r4 'a '(a b)) '(b))
(check-equal? (multirember-r4 'a '(b c)) '(b c))
(check-equal? (multirember-r4 'a '(a b a)) '(b))

;letrecを取る
(define multirember-r5
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) a) (multirember-r5 a (cdr lat)))
          (else (cons (car lat) (multirember-r5 a (cdr lat)))))))

(check-equal? (multirember-r5 'a '()) '())
(check-equal? (multirember-r5 'a '(a b)) '(b))
(check-equal? (multirember-r5 'a '(b c)) '(b c))
(check-equal? (multirember-r5 'a '(a b a)) '(b))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          ((eq? (car lat) a) #t)
          (else (member? a (cdr lat))))))

(check-false (member? 'a '()))
(check-true (member? 'a '(a)))
(check-false (member? 'a '(b)))
(check-true (member? 'a '(b a)))
(check-false (member? 'a '(b b)))

(define member?-r
  (lambda (a lat)
    ((letrec
         ((yes? (lambda (l)
                  (cond ((null? l) #f)
                        ((eq? (car l) a) #t)
                        (else (yes? (cdr l)))))))
       yes?)
     lat)))

(check-false (member?-r 'a '()))
(check-true (member?-r 'a '(a)))
(check-false (member?-r 'a '(b)))
(check-true (member?-r 'a '(b a)))
(check-false (member?-r 'a '(b b)))

(define member?-r2
  (lambda (a lat)
    (letrec
        ((yes? (lambda (l)
                 (cond ((null? l) #f)
                       ((eq? (car l) a) #t)
                       (else (yes? (cdr l)))))))
      (yes? lat))))

(check-false (member?-r2 'a '()))
(check-true (member?-r2 'a '(a)))
(check-false (member?-r2 'a '(b)))
(check-true (member?-r2 'a '(b a)))
(check-false (member?-r2 'a '(b b)))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2) (union (cdr set1) set2))
          (else (cons (car set1) (union (cdr set1) set2))))))

(check-equal? (union '() '()) '())
(check-equal? (union '(a) '()) '(a))
(check-equal? (union '() '(b)) '(b))
(check-equal? (union '(a) '(b)) '(a b))
(check-equal? (union '(a b) '(b)) '(a b))
(check-equal? (union '(a) '(a b)) '(a b))

(define union-r
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond ((null? set) set2)
                    ((member? (car set) set2) (U (cdr set)))
                    (else (cons (car set) (U (cdr set))))))))
      (U set1))))

(check-equal? (union-r '() '()) '())
(check-equal? (union-r '(a) '()) '(a))
(check-equal? (union-r '() '(b)) '(b))
(check-equal? (union-r '(a) '(b)) '(a b))
(check-equal? (union-r '(a b) '(b)) '(a b))
(check-equal? (union-r '(a) '(a b)) '(a b))

(define union-m
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond ((null? set) set2)
                    ((M? (car set) set2) (U (cdr set)))
                    (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (cond ((null? lat) #f)
                     ((eq? (car lat) a) #t)
                     (else (M? a (cdr lat)))))))
      (U set1))))

(check-equal? (union-m '() '()) '())
(check-equal? (union-m '(a) '()) '(a))
(check-equal? (union-m '() '(b)) '(b))
(check-equal? (union-m '(a) '(b)) '(a b))
(check-equal? (union-m '(a b) '(b)) '(a b))
(check-equal? (union-m '(a) '(a b)) '(a b))

(define union-m2
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond ((null? set) set2)
                    ((M? (car set) set2) (U (cdr set)))
                    (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond ((null? lat) #f)
                                ((eq? (car lat) a) #t)
                                (else (N? (cdr lat)))))))
                 (N? lat)))))
      (U set1))))

(check-equal? (union-m2 '() '()) '())
(check-equal? (union-m2 '(a) '()) '(a))
(check-equal? (union-m2 '() '(b)) '(b))
(check-equal? (union-m2 '(a) '(b)) '(a b))
(check-equal? (union-m2 '(a b) '(b)) '(a b))
(check-equal? (union-m2 '(a) '(a b)) '(a b))

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
(check-false (two-in-a-row? '(a b)))
(check-true (two-in-a-row? '(a a)))

(define two-in-a-row-r?
  (lambda (lat)
    (letrec
        ((W (lambda (a lat)
              (cond ((null? lat) #f)
                    (else (or (eq? (car lat) a)
                              (W (car lat) (cdr lat))))))))
      (cond ((null? lat) #f)
            (else (W (car lat) (cdr lat)))))))

(check-false (two-in-a-row-r? '()))
(check-false (two-in-a-row-r? '(a)))
(check-false (two-in-a-row-r? '(a b)))
(check-true (two-in-a-row-r? '(a a)))

(define two-in-a-row-r2?
  (letrec
      ((W (lambda (a lat)
            (cond ((null? lat) #f)
                  (else (or (eq? (car lat) a)
                            (W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
            (else (W (car lat) (cdr lat)))))))

(check-false (two-in-a-row-r2? '()))
(check-false (two-in-a-row-r2? '(a)))
(check-false (two-in-a-row-r2? '(a b)))
(check-true (two-in-a-row-r2? '(a a)))
