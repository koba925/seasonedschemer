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

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
       (cond
         ((> (depth* (cdr l))
             (add1 (depth* (car l))))
          (depth* (cdr l)))
         (else
          (add1 (depth* (car l)))))))))

(check-equal? (depth* '()) 1)
(check-equal? (depth* '(a)) 1)
(check-equal? (depth* '(a b)) 1)
(check-equal? (depth* '((a) b)) 2)
(check-equal? (depth* '(a (b))) 2)
(check-equal? (depth* '(a ())) 2)
(check-equal? (depth* '((a (b)) (c))) 3)
(check-equal? (depth* '((a) (b (c)))) 3)

(define depth*2
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*2 (cdr l)))
      (else
       (let ((a (add1 (depth*2 (car l))))
             (d (depth*2 (cdr l))))
         (cond ((> d a) d)
               (else a)))))))

(check-equal? (depth*2 '()) 1)
(check-equal? (depth*2 '(a)) 1)
(check-equal? (depth*2 '(a b)) 1)
(check-equal? (depth*2 '((a) b)) 2)
(check-equal? (depth*2 '(a (b))) 2)
(check-equal? (depth*2 '(a ())) 2)
(check-equal? (depth*2 '((a (b)) (c))) 3)
(check-equal? (depth*2 '((a) (b (c)))) 3)

(define depth*3
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
       (let ((d (depth*3 (cdr l))))
         (cond
           ((atom? (car l)) d)
           (else
            (let ((a (add1 (depth*3 (car l)))))
              (cond
                ((> d a) d)
                (else a))))))))))

(check-equal? (depth*3 '()) 1)
(check-equal? (depth*3 '(a)) 1)
(check-equal? (depth*3 '(a b)) 1)
(check-equal? (depth*3 '((a) b)) 2)
(check-equal? (depth*3 '(a (b))) 2)
(check-equal? (depth*3 '(a ())) 2)
(check-equal? (depth*3 '((a (b)) (c))) 3)
(check-equal? (depth*3 '((a) (b (c)))) 3)

(define depth*4
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*4 (cdr l)))
      (else
       (let ((a (add1 (depth*4 (car l))))
             (d (depth*4 (cdr l))))
         (if (> d a) d a))))))

(check-equal? (depth*4 '()) 1)
(check-equal? (depth*4 '(a)) 1)
(check-equal? (depth*4 '(a b)) 1)
(check-equal? (depth*4 '((a) b)) 2)
(check-equal? (depth*4 '(a (b))) 2)
(check-equal? (depth*4 '(a ())) 2)
(check-equal? (depth*4 '((a (b)) (c))) 3)
(check-equal? (depth*4 '((a) (b (c)))) 3)

(define max
  (lambda (n m) (if (> n m) n m)))

(define depth*5
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*5 (cdr l)))
      (else
       (let ((a (add1 (depth*5 (car l))))
             (d (depth*5 (cdr l))))
         (max d a))))))

(check-equal? (depth*5 '()) 1)
(check-equal? (depth*5 '(a)) 1)
(check-equal? (depth*5 '(a b)) 1)
(check-equal? (depth*5 '((a) b)) 2)
(check-equal? (depth*5 '(a (b))) 2)
(check-equal? (depth*5 '(a ())) 2)
(check-equal? (depth*5 '((a (b)) (c))) 3)
(check-equal? (depth*5 '((a) (b (c)))) 3)

(define depth*6
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*6 (cdr l)))
      (else
       (max (add1 (depth*6 (car l)))
            (depth*6 (cdr l)))))))

(check-equal? (depth*6 '()) 1)
(check-equal? (depth*6 '(a)) 1)
(check-equal? (depth*6 '(a b)) 1)
(check-equal? (depth*6 '((a) b)) 2)
(check-equal? (depth*6 '(a (b))) 2)
(check-equal? (depth*6 '(a ())) 2)
(check-equal? (depth*6 '((a (b)) (c))) 3)
(check-equal? (depth*6 '((a) (b (c)))) 3)

(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
                ((null? tup) (quote ()))
                (else
                 (let ((rp (cons (car tup) rp)))
                   (cons (pick (car tup) rp)
                         (P (cdr tup) rp))))))))
      (P tup (quote ())))))

(check-equal? (scramble '()) '())
(check-equal? (scramble '(1)) '(1))
(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))

(define leftmost4
  (lambda (l)
    (let/cc skip
      (cond
        ((null? l) (quote ()))
        ((atom? (car l)) (car l))
        (else
         (let ((a (leftmost4 (car l))))
           (cond
             ((atom? a) (skip a))
             (else (leftmost4 (cdr l))))))))))

(check-equal? (leftmost4 '()) '())
(check-equal? (leftmost4 '(a)) 'a)
(check-equal? (leftmost4 '(a b)) 'a)
(check-equal? (leftmost4 '((a) b)) 'a)
(check-equal? (leftmost4 '(() (a) b)) 'a)
(check-equal? (leftmost4 '((()) (a) b)) 'a)
(check-equal? (leftmost4 '((()) (() a) b)) 'a)

(define leftmost5
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) (quote ()))
                   ((atom? (car l)) (skip (car l)))
                   (else
                    (let ((a (lm (car l))))
                      (cond
                        ((atom? a) a)
                        (else (lm (cdr l))))))))))
        (lm l)))))
        
(check-equal? (leftmost5 '()) '())
(check-equal? (leftmost5 '(a)) 'a)
(check-equal? (leftmost5 '(a b)) 'a)
(check-equal? (leftmost5 '((a) b)) 'a)
(check-equal? (leftmost5 '(() (a) b)) 'a)
(check-equal? (leftmost5 '((()) (a) b)) 'a)
(check-equal? (leftmost5 '((()) (() a) b)) 'a)

(define leftmost6
  (lambda (l)
    (let/cc skip
      (lm l skip))))

(define lm
  (lambda (l out)
    (cond ((null? l) (quote ()))
          ((atom? (car l)) (out (car l)))
          (else (let ()
                  (lm (car l) out))
                  (lm (cdr l) out)))))

(check-equal? (leftmost6 '()) '())
(check-equal? (leftmost6 '(a)) 'a)
(check-equal? (leftmost6 '(a b)) 'a)
(check-equal? (leftmost6 '((a) b)) 'a)
(check-equal? (leftmost6 '(() (a) b)) 'a)
(check-equal? (leftmost6 '((()) (a) b)) 'a)
(check-equal? (leftmost6 '((()) (() a) b)) 'a)

(define leftmost7
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond ((null? l) (quote ()))
                       ((atom? (car l)) (skip (car l)))
                       (else (let ()
                               (lm (car l))
                               (lm (cdr l))))))))
        (lm l)))))

(check-equal? (leftmost7 '()) '())
(check-equal? (leftmost7 '(a)) 'a)
(check-equal? (leftmost7 '(a b)) 'a)
(check-equal? (leftmost7 '((a) b)) 'a)
(check-equal? (leftmost7 '(() (a) b)) 'a)
(check-equal? (leftmost7 '((()) (a) b)) 'a)
(check-equal? (leftmost7 '((()) (() a) b)) 'a)

(define leftmost8
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond ((null? l) (quote ()))
                       ((atom? (car l)) (skip (car l)))
                       (else 
                        (lm (car l))
                        (lm (cdr l)))))))
        (lm l)))))

(check-equal? (leftmost8 '()) '())
(check-equal? (leftmost8 '(a)) 'a)
(check-equal? (leftmost8 '(a b)) 'a)
(check-equal? (leftmost8 '((a) b)) 'a)
(check-equal? (leftmost8 '(() (a) b)) 'a)
(check-equal? (leftmost8 '((()) (a) b)) 'a)
(check-equal? (leftmost8 '((()) (() a) b)) 'a)

(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l) (rm a (cdr l) oh))))
      (else
       (if (atom? 
            (let/cc oh
              (rm a (car l) oh)))
           (cons (car l) (rm a (cdr l) oh))
           (cons (rm a (car l) 0) (cdr l)))))))

            

