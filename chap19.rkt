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

(define two-in-a-row-a?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (two-in-a-row-b? (car lat) (cdr lat))))))

(define two-in-a-row-b?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (two-in-a-row-b? (car lat) (cdr lat)))))))

(newline)
(two-in-a-row-a? '(mozzarella cake mozzarella))
(two-in-a-row-a? '(mozzarella mozzarella cake))

(define two-in-a-row?
  (letrec ((W (lambda (a lat)
                (cond ((null? lat) #f)
                      (else (let ((nxt (car lat)))
                              (or (eq? nxt a)
                                  (W nxt (cdr lat)))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
            (else (W (car lat) (cdr lat)))))))

(newline)
(two-in-a-row? '(mozzarella cake mozzarella))
(two-in-a-row? '(mozzarella mozzarella cake))

(define leave #f)
(define walk
  (lambda (l)
    (cond ((null? l) (quote ()))
          ((atom? (car l)) (leave (car l)))
          (else (let ()
                  (walk (car l)) (walk (cdr l)))))))

(define start-it
  (lambda (l)
    (let/cc here
      (set! leave here)
      (walk l))))

(newline)
(start-it '((potato) (chips (chips (with))) fish))

(define fill #f)
(define waddle
  (lambda (l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (let ()
             (let/cc rest
               (set! fill rest)
               (leave (car l)))
             (waddle (cdr l))))
          (else (let ()
                  (waddle (car l))
                  (waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l))))

(define get-next
  (lambda (x)
    (let/cc here-again
      (set! leave here-again)
      (fill (quote go)))))

(newline)
(start-it2 '((donuts) (cheerios (cheerios (spaghettios))) donuts))
(get-next (quote go))
(get-next (quote go))
(get-next (quote go))
(get-next (quote go))
(get-next (quote go))

;(newline)
;(let ()
;  (display "1 ") (display (start-it2 '(a b))) (newline)
;  (display "2 ") (display (get-next (quote go))) (newline)
;  (display "3 ") (display (get-next (quote go))) (newline))

;(define flat
;  (lambda (l)
;    (let ((a (let/cc here
;               (set! leave here)
;               (waddle l))))
;      (cond ((null? a) #f)
;            (else
;             (display a) (newline)
;             (fill (quote ())))))))
;
;(newline)
;(flat '((donuts) (cheerios (cheerios (spaghettios))) donuts))

(define get-first
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l)
      (leave (quote())))))

(newline)
(let ()
  (display "1 ") (display (get-first '(a b))) (newline)
  (display "2 ") (display (get-next (quote go))) (newline)
  (display "3 ") (display (get-next (quote go))) (newline))

(define A
  (lambda ()
    (let/cc here (set! leave here)
      (display "A1") (newline)
      (B))
    (let/cc here (set! leave here)
      (display "A2") (newline)
      (fill))
    (let/cc here (set! leave here)
      (display "A3") (newline)
      (fill))
    (display "A4") (newline)))

(define B
  (lambda ()
    (let/cc here (set! fill here)
      (display "B1") (newline)
      (leave))
    (let/cc here (set! fill here)
      (display "B2") (newline)
      (leave))
    (let/cc here (set! fill here)
      (display "B3") (newline)
      (leave))))

(newline)
(A)

;(define waddle&co
;  (lambda (l col)
;    (cond ((null? l) (col (quote ())))
;          ((atom? (car l))
;           (display (car l)) (newline)
;           (waddle&co (cdr l) col))
;          (else (waddle&co (car l) (lambda (x) (waddle&co (cdr l) col)))))))
;
;(newline)
;(waddle&co '() (lambda (x) x))
;((lambda (x) x) (quote ()))
;(quote ())
;
;(newline)
;(waddle&co '(a) (lambda (x) x))
;; (display 'a) (newline)
;(waddle&co '() (lambda (x) x))
;((lambda (x) x) (quote ()))
;(quote ())
;
;(newline)
;(waddle&co '(a b) (lambda (x) x))
;; (display 'a) (newline)
;(waddle&co '(b) (lambda (x) x))
;; (display 'b) (newline)
;(waddle&co '() (lambda (x) x))
;((lambda (x) x) (quote ()))
;(quote ())
;
;(newline)
;(waddle&co '((a) b) (lambda (x) x))
;(waddle&co '(a) (lambda (x) (waddle&co '(b) (lambda (x) x))))
;; (display 'a) (newline)
;(waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))
;((lambda (x) (waddle&co '(b) (lambda (x) x))) (quote ()))
;(waddle&co '(b) (lambda (x) x))
;; (display 'b) (newline)
;(waddle&co '(b) (lambda (x) x))
;(waddle&co '() (lambda (x) x))
;((lambda (x) x) (quote ()))
;(quote ())
;
;(newline)
;(waddle&co '(((a)) b) (lambda (x) x))
;(waddle&co '((a)) (lambda (x) (waddle&co '(b) (lambda (x) x))))
;(waddle&co '(a) (lambda (x) (waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))))
;; (display 'a) (newline)
;(waddle&co '() (lambda (x) (waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))))
;((lambda (x) (waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))) (quote ()))
;(waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))
;((lambda (x) (waddle&co '(b) (lambda (x) x))) (quote ()))
;(waddle&co '(b) (lambda (x) x))
;; (display 'b) (newline)
;(waddle&co '() (lambda (x) x))
;((lambda (x) x) (quote ()))
;(quote ())
;
;(newline)
;(waddle&co '((donuts) (cheerios (cheerios (spaghettios))) donuts) (lambda (x) x))
;
;(define append
;  (lambda (l a)
;    (cond ((null? l) (cons a (quote())))
;          (else (cons (car l) (append (cdr l) a))))))
;
;(define waddle2&co
;  (lambda (l col)
;    (cond ((null? l) (col (quote ())))
;          ((atom? (car l))
;           (waddle2&co (cdr l) (lambda (x) (append (col x) (car l)))))
;          (else (waddle2&co (cdr l) (lambda (x) (waddle2&co (car l) col)))))))
;
;(newline)
;(waddle2&co '() (lambda (x) x))
;((lambda (x) x) (quote ()))
;(quote ())
;
;(newline)
;(waddle2&co '(a) (lambda (x) x))
;(waddle2&co '() (lambda (x) (cons 'a ((lambda (x) x) x))))
;((lambda (x) (cons 'a ((lambda (x) x) x))) (quote ()))
;(cons 'a ((lambda (x) x) (quote ())))
;(cons 'a (quote ()))
;'(a)
;
;(newline)
;(waddle2&co '(a b) (lambda (x) x))
;(waddle2&co '((a) b) (lambda (x) x))
;(waddle2&co '(((a)) (b (c))) (lambda (x) x))

(define get-first&co
  (lambda (l)
    (let ((here (lambda (x) x)))
      (set! leave here)
      (waddle&co l here))))

(define get-next&co
  (lambda ()
    (let ((here-again (lambda (x) x)))
      (set! leave here-again)
      (fill (quote go)))))

(define waddle&co
  (lambda (l col)
    (cond ((null? l) (col (quote ())))
          ((atom? (car l))
           (let ((rest (lambda (x) (waddle&co (cdr l) col))))
             (set! fill rest)
             (leave (car l))))
          (else (waddle&co
                 (car l)
                 (lambda (x) (waddle&co (cdr l) col)))))))

(newline)
(get-first&co '(((a)) b))
(get-next&co)
(get-next&co)

(get-first&co '(((a)) b))
; (set leave (lambda (x) x))
(waddle&co '(((a)) b) (lambda (x) x))
(waddle&co '((a)) (lambda (x) (waddle&co '(b) (lambda (x) x))))
(waddle&co '(a) (lambda (x) (waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))))
; (set fill (lambda (x) (waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))))
(leave 'a)
((lambda (x) x) 'a)
'a

(get-next&co)
; (set leave (lambda (x) x))
(fill (quote go))
((lambda (x) (waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))) (quote go))
(waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))
((lambda (x) (waddle&co '(b) (lambda (x) x))) (quote ()))
(waddle&co '(b) (lambda (x) x))
; (set fill (lambda (x) (waddle&co '() (lambda (x) x))))
(leave 'b)
((lambda (x) x) 'b)
'b

(get-next&co)
; (set leave (lambda (x) x))
(fill (quote go))
((lambda (x) (waddle&co '() (lambda (x) x))) (quote go))
(waddle&co '() (lambda (x) x))
((lambda (x) x) (quote ()))
(quote ())
'()

(newline)
(get-first&co '((donuts) (cheerios (cheerios (spaghettios))) donuts))
(get-next&co)
(get-next&co)
(get-next&co)
(get-next&co)
(get-next&co)

;(define two-in-a-row*?
;  (lambda (l)
;    (let ((fst (get-first l)))
;      (if (atom? fst)
;          (two-in-a-row-b*? fst)
;          #f))))
;
;(define two-in-a-row-b*?
;  (lambda (a)
;    (let ((n (get-next (quote go))))
;      (if (atom? n)
;          (or (eq? n a)
;              (two-in-a-row-b*? n))
;          #f))))

(define two-in-a-row*?
  (letrec
      ((T? (lambda (a)
             (let ((n (get-next (quote ()))))
               (if (atom? n)
                   (or (eq? n a) (T? n))
                   #f))))
       (get-next (lambda (x)
                   (let/cc here-again
                     (set! leave here-again)
                     (fill (quote go)))))
       (fill (lambda (x) x))
       (waddle (lambda (l)
                 (cond ((null? l) (quote ()))
                       ((atom? (car l))
                        (let ()
                          (let/cc rest
                            (set! fill rest)
                            (leave (car l)))
                          (waddle (cdr l))))
                       (else (let ()
                               (waddle (car l))
                               (waddle (cdr l)))))))
       (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (let/cc here
                   (set! leave here)
                   (waddle l)
                   (leave (quote ())))))
        (if (atom? fst) (T? fst) #f)))))

(newline)
(two-in-a-row*? '())
(two-in-a-row*? '((donuts) (cheerios ((spaghettios))) donuts))
(two-in-a-row*? '((donuts) (cheerios (cheerios (spaghettios))) donuts))
(two-in-a-row*? '((donuts donuts) (cheerios ((spaghettios))) donuts))

(define two-in-a-row2*?
  (lambda (l)
    (let ((prv (quote ())))
      (letrec ((T (lambda (l)
                    (cond ((null? l) #f)
                          ((atom? (car l))
                           (cond ((eq? (car l) prv) #t)
                                 (else
                                  (set! prv (car l))
                                  (T (cdr l)))))
                          (else
                           (or (T (car l)) (T (cdr l))))))))
        (T l)))))

(newline)
(two-in-a-row2*? '())
(two-in-a-row2*? '((donuts) (cheerios ((spaghettios))) donuts))
(two-in-a-row2*? '((donuts) (cheerios (cheerios (spaghettios))) donuts))
(two-in-a-row2*? '((donuts donuts) (cheerios ((spaghettios))) donuts))

(define copy
  (lambda (l)
    (cond ((null? l) (quote()))
          ((atom? (car l)) (cons (car l) (copy (cdr l))))
          (else (cons (copy (car l)) (copy (cdr l)))))))

(newline)
(copy '())
(copy '(a))
(copy '(a b))
(copy '((a) b))
(copy '(a (b)))
(copy '(a () (())))

;(define rest
;  (lambda (l)
;    (cond ((null? l) (quote()))
;          ((atom? (car l)) (cdr l))
;          (else (let ((ar (rest (car l))))
;                  (cond ((equal? (car l) ar) (rest (cdr l))) ;必要か？
;                        ((null? ar) (cdr l))
;                        (else (cons ar (cdr l)))))))))

(define pair
  (lambda (a b)
    (cons a (cons b (quote ())))))
(define 1st car)
(define rest cadr)

(define 1st-and-rest
  (lambda (l)
    (cond ((null? l) (pair (quote ()) (quote())))
          ((atom? (car l)) (pair (car l) (cdr l)))
          (else (let ((a1r (1st-and-rest (car l))))
                  (let ((a1 (1st a1r))
                        (ar (rest a1r)))
                    (cond ((null? a1) (1st-and-rest (cdr l)))
                          (else (pair a1 (cons ar (cdr l)))))))))))
  
(newline)
(1st-and-rest '(a b))
(1st-and-rest '((a b) c))
(1st-and-rest '(a (b c)))
(1st-and-rest '(() a b))
(1st-and-rest '((()) a (b)))

(newline)
(1st-and-rest '((donuts) () (()) (() cheerios (cheerios (()) (spaghettios))) donuts))
(1st-and-rest '(() (()) (() cheerios (cheerios (()) (spaghettios))) donuts))
(1st-and-rest '(((cheerios (()) (spaghettios))) donuts))
(1st-and-rest '((((()) (spaghettios))) donuts))
(1st-and-rest '(((())) donuts))
(1st-and-rest '(((()))))
(1st-and-rest '())

(define 1st-and-rest2
  (lambda (l)
    (cond ((null? l) (pair (quote ()) (quote())))
          ((atom? (car l)) (pair (car l) (cdr l)))
          (else (let ((a1r (1st-and-rest2 (car l))))
                  (let ((a1 (1st a1r))
                        (ar (rest a1r)))
                    (cond ((null? a1) (1st-and-rest2 (cdr l)))
                          ((null? ar) (pair a1 (cdr l)))
                          (else (pair a1 (cons ar (cdr l)))))))))))

(newline)
(1st-and-rest2 '(a b))
(1st-and-rest2 '((a b) c))
(1st-and-rest2 '(a (b c)))
(1st-and-rest2 '(() a b))
(1st-and-rest2 '((()) a (b)))

(newline)
(1st-and-rest2 '((donuts) () (()) (() cheerios (cheerios (()) (spaghettios))) donuts))
(1st-and-rest2 '(() (()) (() cheerios (cheerios (()) (spaghettios))) donuts))
(1st-and-rest2 '(((cheerios (()) (spaghettios))) donuts))
(1st-and-rest2 '((((()) (spaghettios))) donuts))
(1st-and-rest2 '(donuts))

(define two-in-a-row3*?
  (lambda (l)
    (letrec ((T (lambda (p l)
                  (let ((1r (1st-and-rest l)))
                    (cond ((null? (1st 1r)) #f)
                          ((eq? (1st 1r) p) #t)
                          (else (T (1st 1r) (rest 1r))))))))
      (T (quote ()) l))))

(newline)
(two-in-a-row3*? '((donuts) () (()) (() cheerios (cheerios (()) (spaghettios))) donuts))
(two-in-a-row3*? '((donuts) () (()) (() (cheerios (()) (spaghettios))) donuts))

(define val car)
(define prv cadr)

(define two-in-a-row4*?
  (lambda (l)
    (letrec ((T (lambda (p l)
                  (cond ((null? l) (pair #f (quote())))
                        ((atom? (car l))
                         (cond ((eq? (car l) p) (pair #t (quote ())))
                               (else (T (car l) (cdr l)))))
                        (else (let ((vp (T p (car l))))
                                (cond ((val vp) (pair #t (quote ())))
                                      (else (T (prv vp) (cdr l))))))))))
      (val (T (quote ()) l)))))

(newline)
(two-in-a-row4*? '())
(two-in-a-row4*? '((donuts) (cheerios ((spaghettios))) donuts))
(two-in-a-row4*? '((donuts) (cheerios (cheerios (spaghettios))) donuts))
(two-in-a-row4*? '((donuts donuts) (cheerios ((spaghettios))) donuts))