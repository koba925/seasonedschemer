#lang racket

(require "util.rkt")

(define bons
  (lambda (kar)
    (let ((kdr (quote ())))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define set-kdr
  (lambda (c x) ((c (lambda (s a d) s)) x)))

(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

(bons 1)
(kar (bons 1))
(kdr (bons 1))
(set-kdr (bons 1) 2)
(let ((c (bons 1)))
  (set-kdr c 2)
  (kdr c))

(define kounter #f)
(define set-kounter #f)
(define konsC
  (let ((kount 0))
    (set! kounter (lambda () kount))
    (set! set-kounter (lambda (x) (set! kount x)))
    (lambda (kar kdr)
      (set! kount (add1 kount))
      (kons kar kdr))))

(newline)
(konsC 1 2)
(kounter)
(konsC 1 2)
(konsC 1 2)
(kounter)
(set-kounter 0)
(kounter)

(define adom?
  (lambda (s)
    (and (atom? s) (not (procedure? s)))))

(define wride
  (lambda (l)
    (letrec
        ((R (lambda (l)
              (W (kar l))
              (cond ((null? (kdr l)))
                    ((adom? (kdr l))
                     (display " . ")
                     (write (kdr l)))
                    (else
                     (display " ")
                     (R (kdr l))))))
         (W (lambda (l)
              (cond ((adom? l) (write l))
                    ((null? l) (write l))
                    (else
                     (display "(")
                     (R l)
                     (display ")"))))))
      (W l)
      (newline))))

(newline)
(write (cons 1 2)) (newline)
(wride (kons 1 2))
(write (cons '() '())) (newline)
(wride (kons '() '()))
(write (cons "a" (cons "b" "c"))) (newline)
(wride (kons "a" (kons "b" "c")))
(write (cons (cons 'a 'b) 'c)) (newline)
(wride (kons (kons 'a 'b) 'c))
(write (cons 1 (cons (cons 2 (cons 3 '())) '()))) (newline)
(wride (kons 1 (kons (kons 2 (kons 3 '())) '())))
(write (cons (cons 1 (cons 2 '())) (cons 3 '()))) (newline)
(wride (kons (kons 1 (kons 2 '())) (kons 3 '())))

(newline)
(define l (kons 1 (kons 2 '())))
(wride l)
(set-kdr l (kons 2 (kons 3 '())))
(wride l)
(wride (kdr l))
(wride (kar (kdr l)))

(define gons
  (lambda (kar kdr)
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr))))

(newline)
(define m (gons 1 (gons 2 '())))
(wride m)
(set-kdr m (gons 2 (gons 3 '())))
(wride m)
(wride (kdr m))
(wride (kar (kdr m)))

(define lots
  (lambda (m)
    (cond ((zero? m) (quote ()))
          (else (konsC (quote egg) (lots (sub1 m)))))))

(newline)
(wride (lots 0))
(wride (lots 3))

(define lenkth
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (lenkth (kdr l)))))))

(newline)
(lenkth (lots 0))
(lenkth (lots 3))

(define add-at-end
  (lambda (l)
    (cond ((null? (kdr l))
           (konsC (kar l) (konsC (quote egg) (quote ()))))
          (else
           (konsC (kar l) (add-at-end (kdr l)))))))

(newline)
(wride (add-at-end (lots 3)))
(kounter)

(define add-at-end-too
  (lambda (l)
    (letrec ((A (lambda (l)
                  (cond ((null? (kdr l))
                         (set-kdr l (konsC (quote egg) (quote ()))))
                        (else
                         (A (kdr l)))))))
      (A l)
      l)))

(newline)
(set-kounter 0)
(wride (add-at-end-too (lots 3)))
(kounter)

(newline)
(set-kounter 0)
(define dozen (lots 12))
(kounter)
(define bakers-dozen (add-at-end dozen))
(kounter)
(define bakers-dozen-too (add-at-end-too dozen))
(kounter)
(define bakers-dozen-again (add-at-end dozen))
(kounter)

(define eklist?
  (lambda (ls1 ls2)
    (cond ((null? ls1) (null? ls2))
          ((null? ls2) #f)
          (else
           (and (eq? (kar ls1) (kar ls2))
                (eklist? (kdr ls1) (kdr ls2)))))))

(newline)
(eklist? dozen bakers-dozen)
(eklist? dozen bakers-dozen-too)
(eklist? dozen bakers-dozen-again)
(eklist? bakers-dozen bakers-dozen-too)

(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))

(newline)
(same? dozen bakers-dozen-too)
(same? bakers-dozen bakers-dozen-too)

(define last-kons
  (lambda (ls)
    (cond ((null? (kdr ls)) ls)
          (else (last-kons (kdr ls))))))

(define finite-lenkth
  (lambda (p)
    (let/cc infinite
      (letrec ((C (lambda (p q)
                    (cond ((same? p q) (infinite #f))
                          ((null? q) 0)
                          ((null? (kdr q)) 1)
                          (else (+ (C (sl p) (qk q)) 2)))))
               (qk (lambda (x) (kdr (kdr x))))
               (sl (lambda (x) (kdr x))))
        (cond ((null? p) 0)
              (else (add1 (C p (kdr p)))))))))

(define finite-lenkth1
  (lambda (p)
    (letrec ((C (lambda (q)
                  (cond ((null? q) 0)
                        ((null? (kdr q)) 1)
                        (else (+ (C (qk q)) 2)))))
             (qk (lambda (x) (kdr (kdr x)))))
      (cond ((null? p) 0)
            (else (add1 (C (kdr p))))))))

(define lenkth2
  (lambda (q)
    (cond ((null? q) 0)
          ((null? (kdr q)) 1)
          (else (+ (lenkth2 (kdr (kdr q))) 2)))))

(define long (lots 12))

(newline)
(wride long)
(wride (last-kons long))
(lenkth long)
(finite-lenkth long)
(finite-lenkth1 long)
(lenkth2 long)

(set-kdr (last-kons long) long)
(finite-lenkth long)

(set! long (lots 12))
(set-kdr (last-kons long) (kdr (kdr long)))
;(wride long)
;(lenkth long)
(finite-lenkth long)

