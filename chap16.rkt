#lang racket/base

(require rackunit)
(require "util.rkt")

(define sweet-tooth
  (lambda (food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(define last (quote angelfood))

(sweet-tooth (quote chocolate))
last

(sweet-tooth (quote fruit))
last

(newline)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(sweet-toothL (quote chocolate))
last

(sweet-toothL (quote fruit))
last

(sweet-toothL (quote cheese))
(sweet-toothL (quote carrot))

(newline)

(define ingredients (quote ()))
(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food
          (cons (quote cake)
                (quote ())))))

(sweet-toothR (quote chocolate))
ingredients
(sweet-toothR (quote fruit))
ingredients
(sweet-toothR (quote cheese))
ingredients
(sweet-toothR (quote carrot))
ingredients

(newline)

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond ((null? ns) #f)
                    ((= (car ns) n) (car rs))
                    (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deep
  (lambda (m)
    (cond ((zero? m) (quote pizza))
          (else (cons (deepM (sub1 m)) (quote ()))))))

(define deepM
  (let ((Ns (quote ()))
        (Rs (quote ())))
    (lambda (n)
      (let ((found (find n Ns Rs)))
        (if found
            found
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result))))))

(newline)
(deepM 0)
(deepM 1)
(deepM 3)

(define L
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h (L (lambda (arg) (h arg))))
    h))

(length '())
(length '(a b))

(let ((h (lambda (l) 0)))
    (set! h (L (lambda (arg) (h arg))))
(h '(a b))
((L (lambda (arg) (h arg))) '(a b))
((lambda (l) (cond ((null? l) 0) (else (add1 ((lambda (arg) (h arg)) (cdr l)))))) '(a b))
(add1 ((lambda (arg) (h arg)) '(b)))
(add1 (h '(b)))
(add1 ((L (lambda (arg) (h arg))) '(b)))
(add1 ((lambda (l) (cond ((null? l) 0) (else (add1 ((lambda (arg) (h arg)) (cdr l)))))) '(b)))
(add1 (add1 ((lambda (arg) (h arg)) '())))
(add1 (add1 (h '())))
(add1 (add1 ((L (lambda (arg) (h arg))) '())))
(add1 (add1 ((lambda (l) (cond ((null? l) 0) (else (add1 ((lambda (arg) (h arg)) (cdr l)))))) '())))
(add1 (add1 0))
(add1 1)
2
)

(define Y!
  (lambda (L)
    (let ((h (lambda (l) (quote ()))))
      (set! h (L (lambda (arg) (h arg))))
      h)))

(println "Y! L")
((Y! L) '())
((Y! L) '(a b))
(((lambda (L)
    (let ((h (lambda (l) (quote ()))))
      (set! h (L (lambda (arg) (h arg))))
      h))
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '(a b))

((let ((h (lambda (l) (quote ()))))
      (set! h ((lambda (length)
                 (lambda (l)
                   (cond ((null? l) 0)
                         (else (add1 (length (cdr l)))))))
               (lambda (arg) (h arg))))
      h)  
 '(a b))

((let ((h (lambda (l) (quote ()))))
      (set! h ((lambda (length)
                 (lambda (l)
                   (cond ((null? l) 0)
                         (else (add1 (length (cdr l)))))))
               (lambda (arg) (h arg))))
      h)  
 '(a b))

(println "Y L")
((Y L) '())
((Y L) '(a b))

(((lambda (le)
    ((lambda (g) (g g))
     (lambda (g) (le (lambda (x) ((g g) x))))))
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '(a b))

(((lambda (g) (g g))
  (lambda (g)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     (lambda (x) ((g g) x)))))
 '(a b))

(((lambda (g)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     (lambda (x) ((g g) x))))
  (lambda (g)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     (lambda (x) ((g g) x)))))
 '(a b))

(define Y-bang
  (lambda (f)
    (letrec ((h (f (lambda (arg) (h arg)))))
      h)))

(println "Y-bang L")
((Y-bang L) '())
((Y-bang L) '(a b))

(define D
  (lambda (depth*)
    (lambda (s)
      (cond ((null? s) 1)
            ((atom? (car s)) (depth* (cdr s)))
            (else (max (add1 (depth* (car s))) (depth* (cdr s))))))))

(define depth* (Y! D))

(depth* '())
(depth* '(a a))
(depth* '((a a) b))
(depth* '(a (b b)))

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a) (if (= a x) 0 (f a))))))

;((Y! biz) 5)
;(((lambda (L)
;    (let ((h (lambda (l) 0)))
;      (set! h (L (lambda (arg) (h arg))))
;      h))
;  (lambda (f)
;      (set! x (add1 x))
;      (lambda (a) (if (= a x) 0 (f a)))))
; 5)

(define YY
  (lambda (le)
    ((lambda (g) (g g))
     (lambda (g) (le (lambda (x) ((g g) x)))))))

((YY biz) 5)

(let ((x 0))
  (((lambda (le)
      ((lambda (g) (g g))
       (lambda (g) (le (lambda (x) ((g g) x))))))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a) (if (= a x) 0 (f a)))))
   5)
  )

(let ((x 0))
  (((lambda (g) (g g))
    (lambda (g) ((lambda (f)
                   (set! x (add1 x))
                   (lambda (a) (if (= a x) 0 (f a)))) (lambda (x) ((g g) x)))))
   5)
  )

(let ((x 0))
  (((lambda (g) ((lambda (f)
                   (set! x (add1 x))
                   (lambda (a) (if (= a x) 0 (f a)))) (lambda (x) ((g g) x))))
    (lambda (g) ((lambda (f)
                   (set! x (add1 x))
                   (lambda (a) (if (= a x) 0 (f a)))) (lambda (x) ((g g) x)))))
   5)
  )

(define a1
  (lambda (n) (+ n 1)))

(a1 10)
((lambda (n) (+ n 1)) 10)

(define a2
  (lambda (f)
    (lambda (n) (f (f n)))))

((a2 a1) 10)
(((lambda (f)
    (lambda (n) (f (f n))))
  (lambda (n) (+ n 1)))
 10)
((lambda (n)
   ((lambda (n) (+ n 1))
    ((lambda (n) (+ n 1)) n)))  
 10)
((lambda (n) (+ n 1))
 ((lambda (n) (+ n 1)) 10))  
