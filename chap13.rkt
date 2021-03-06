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
(check-equal? (intersectall '((a b) (c d) (e f) (g h))) '())

; (intersectall-c '((a b c) () (b e c)))
; (let/cc hop (A '((a b c) () (b e c))))
; (let/cc hop (intersect '(a b c) (A '(() (b e c)))))
; (let/cc hop (intersect '(a b c) (hop (quote ()))))
; (let/cc hop (quote ()))
; (quote ())

;(intersectall '((a b) (c d) (e f) (g h)))
;(let/cc hop (A '((a b) (c d) (e f) (g h))))
;(let/cc hop (I '(a b) (A '((c d) (e f) (g h)))))
;(let/cc hop (I '(a b) (I '(c d) (A '((e f) (g h))))))
;(let/cc hop (I '(a b) (I '(c d) (I '(e f) (A '((g h)))))))
;(let/cc hop (I '(a b) (I '(c d) (I '(e f) '(g h)))))
;(let/cc hop (I '(a b) (I '(c d) '())))
;(let/cc hop (I '(a b) (hop (quote ()))))
;(let/cc hop (quote ()))
;(quote ())

(define intersect2
  (lambda (set1 set2 hop)
    (letrec
        ((I (lambda (set)
              (cond ((null? set) (quote ()))
                    ((member? (car set) set2)
                     (cons (car set) (I (cdr set))))
                    (else (I (cdr set)))))))
      (cond ((null? set2) (hop (quote ())))
            (else (I set1))))))

(define intersectall2
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond ((null? (car lset))
                       (hop (quote ())))
                      ((null? (cdr lset)) (car lset))
                      (else (intersect2 (car lset) (A (cdr lset)) hop))))))
        (cond ((null? lset) (quote()))
              (else (A lset)))))))

(check-equal? (intersectall2 '()) '())
(check-equal? (intersectall2 '(())) '())
(check-equal? (intersectall2 '((a))) '(a))
(check-equal? (intersectall2 '((a) (a))) '(a))
(check-equal? (intersectall2 '((a) (b))) '())
(check-equal? (intersectall2 '((a) (a) (a))) '(a))
(check-equal? (intersectall2 '((a) (a) (b))) '())
(check-equal? (intersectall2 '((a b c) (c a d) (b e c))) '(c))
(check-equal? (intersectall2 '((a b c) () (b e c))) '())
(check-equal? (intersectall2 '((a b) (c d) (e f) (g h))) '())

(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a) (cdr lat))
                (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(check-equal? (rember 'a '()) '())
(check-equal? (rember 'a '(a b c)) '(b c))
(check-equal? (rember 'a '(b a c)) '(b c))
(check-equal? (rember 'a '(a b a)) '(b a))

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a) (quote ()))
                (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(check-equal? (rember-beyond-first 'a '()) '())
(check-equal? (rember-beyond-first 'a '(a)) '())
(check-equal? (rember-beyond-first 'a '(a b)) '())
(check-equal? (rember-beyond-first 'a '(b a c)) '(b))
(check-equal? (rember-beyond-first 'a '(b c d)) '(b c d))

;これではダメ
;毎回hopに継続を保存してしまうからhopしても意味がなくなっている
;(define rember-upto-last
;  (lambda (a lat)
;    (let/cc hop
;      (cond ((null? lat) (quote ()))
;            ((eq? (car lat) a) (hop (rember-upto-last a (cdr lat))))
;            (else (cons (car lat) (rember-upto-last a (cdr lat))))))))
;手で展開して確かめる

;(define r
;  (lambda (a lat)
;    (let/cc hop
;      (cond ((null? lat) (quote ()))
;            ((eq? (car lat) a) (hop (r a (cdr lat))))
;            (else (cons (car lat) (r a (cdr lat))))))))
;
;(r 'a '(b a c a d))
;(let/cc hop (cons 'b (r 'a '(a c a d))))
;(let/cc hop (cons 'b (let/cc hop (hop (r 'a '(c a d))))))
;(let/cc hop (cons 'b (let/cc hop (hop (let/cc hop (cons 'c (r 'a '(a d))))))))
;(let/cc hop (cons 'b (let/cc hop (hop (let/cc hop (cons 'c (let/cc hop (hop (r 'a '(d))))))))))
;(let/cc hop (cons 'b (let/cc hop (hop (let/cc hop (cons 'c (let/cc hop (hop (let/cc hop (cons 'd (r 'a '())))))))))))
;(let/cc hop (cons 'b (let/cc hop (hop (let/cc hop (cons 'c (let/cc hop (hop (let/cc hop (cons 'd (let/cc hop (quote ()))))))))))))
;(let/cc hop (cons 'b (let/cc hop (hop (let/cc hop (cons 'c (let/cc hop (hop (let/cc hop (cons 'd '()))))))))))
;(let/cc hop (cons 'b (let/cc hop (hop (let/cc hop (cons 'c (let/cc hop (hop (let/cc hop '(d))))))))))
;(let/cc hop (cons 'b (let/cc hop (hop (let/cc hop (cons 'c (let/cc hop (hop '(d)))))))))
;(let/cc hop (cons 'b (let/cc hop (hop (let/cc hop (cons 'c '(d)))))))
;(let/cc hop (cons 'b (let/cc hop (hop (let/cc hop '(c d))))))
;(let/cc hop (cons 'b (let/cc hop '(c d))))
;(let/cc hop (cons 'b '(c d)))
;(let/cc hop '(b c d))
;'(b c d)

(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
      (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) (quote ()))
                  ((eq? (car lat) a) (skip (R (cdr lat))))
                  (else (cons (car lat) (R (cdr lat))))))))
        (R lat)))))
  
(check-equal? (rember-upto-last 'a '()) '())
(check-equal? (rember-upto-last 'a '(a)) '())
(check-equal? (rember-upto-last 'a '(a b)) '(b))
(check-equal? (rember-upto-last 'a '(b a c)) '(c))
(check-equal? (rember-upto-last 'a '(b c d)) '(b c d))
(check-equal? (rember-upto-last 'a '(b a c a d)) '(d))

(define rember-upto-last&co
  (lambda (a lat)
    (letrec
        ((R (lambda (lat col)
              (cond
                ((null? lat) (col (quote ())))
                ((eq? (car lat) a)
                 (R (cdr lat) (lambda (x) x)))
                (else
                 (R (cdr lat) (lambda (x) (col (cons (car lat) x)))))))))
      (R lat (lambda (x) x)))))

(check-equal? (rember-upto-last&co 'a '()) '())
(check-equal? (rember-upto-last&co 'a '(a)) '())
(check-equal? (rember-upto-last&co 'a '(a b)) '(b))
(check-equal? (rember-upto-last&co 'a '(b a c)) '(c))
(check-equal? (rember-upto-last&co 'a '(b c d)) '(b c d))
(check-equal? (rember-upto-last&co 'a '(b c a b c a c d)) '(c d))

;(rember-upto-last 'a '(b a c a d))
;(let/cc skip (R '(b a c a d)))
;(let/cc skip (cons 'b (R '(a c a d))))
;(let/cc skip (cons 'b (skip (R '(c a d)))))
;(let/cc skip (cons 'b (skip (cons 'c (R '(a d))))))
;(let/cc skip (cons 'b (skip (cons 'c (skip (R '(d)))))))
;(let/cc skip (cons 'b (skip (cons 'c (skip (cons 'd (R '())))))))
;(let/cc skip (cons 'b (skip (cons 'c (skip (cons 'd '()))))))
;(let/cc skip (cons 'b (skip (cons 'c (skip '(d))))))
;(let/cc skip '(d))
;'(d)

(define my-rember-upto-last
  (lambda (a lat)
    (letrec
        ((R (lambda (rlat result)
              (cond ((null? rlat) result)
                    ((eq? (car rlat) a) result)
                    (else (R (cdr rlat) (cons (car rlat) result)))))))
    (R (reverse lat) (quote ())))))

(check-equal? (my-rember-upto-last 'a '()) '())
(check-equal? (my-rember-upto-last 'a '(a)) '())
(check-equal? (my-rember-upto-last 'a '(a b)) '(b))
(check-equal? (my-rember-upto-last 'a '(b a c)) '(c))
(check-equal? (my-rember-upto-last 'a '(b c d)) '(b c d))
(check-equal? (my-rember-upto-last 'a '(b a c a d)) '(d))

