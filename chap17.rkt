#lang racket

(require "util.rkt")

(define find
  (lambda (n Ns Rs)
    (cond ((null? Ns) #f)
          ((eq? (car Ns) n) (car Rs))
          (else (find n (cdr Ns) (cdr Rs))))))

(find 3 '(1 2 3 4) '(a b c d))
(find 0 '(1 2 3 4) '(a b c d))

(define counter #f)
(define set-counter #f)
(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deep
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (consC (deep (sub1 m)) (quote ())))))

(deep 5)
(counter)
(deep 7)
(counter)

(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(set-counter 0)
(supercounter deep)

(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       (quote pizza)
                       (consC (deepM (sub1 n)) (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(set-counter 0)
(supercounter deepM)

(define rember1*C
  (lambda (a l)
    (letrec ((R (lambda (l oh)
                  (cond ((null? l) (oh (quote no)))
                        ((atom? (car l))
                         (if (eq? (car l) a)
                             (cdr l)
                             (consC (car l) (R (cdr l) oh))))
                        (else
                         (let ((new-car (let/cc oh (R (car l) oh))))
                           (if (atom? new-car)
                               (consC (car l) (R (cdr l) oh))
                               (consC new-car (cdr l)))))))))
      (let ((new-l (let/cc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))

(set-counter 0)
(rember1*C 'noodles '((food) more (food)))
(counter)

(define rember1*C2
  (lambda (a l)
    (letrec ((R (lambda (l)
                  (cond ((null? l) (quote ()))
                        ((atom? (car l))
                         (if (eq? (car l) a)
                             (cdr l)
                             (consC (car l) (R (cdr l)))))
                        (else
                         (let ((av (R (car l))))
                           (if (eqlist? (car l) av)
                               (consC (car l) (R (cdr l)))
                               (consC av (cdr l)))))))))
      (R l))))

(set-counter 0)
(rember1*C2 'noodles '((food) more (food)))
(counter)