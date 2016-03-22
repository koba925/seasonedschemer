#lang racket

(require rackunit)
(require "util.rkt")

(define text-of (lambda (x) (car (cdr x))))
(define formals-of (lambda (x) (car (cdr x))))
(define body-of (lambda (x) (cdr (cdr x))))
(define ccbody-of (lambda (x) (cdr (cdr x))))
(define name-of (lambda (x) (car (cdr x))))
(define right-side-of
  (lambda (x)
    (cond ((null? (cdr (cdr x))) 0)
          (else (car (cdr (cdr x)))))))
(define cond-lines-of (lambda (x) (cdr x)))
(define else?
  (lambda (x)
    (cond ((atom? x) (eq? x (quote else)))
          (else #f))))
(define question-of (lambda (x) (car x)))
(define answer-of (lambda (x) (car (cdr x))))
(define function-of (lambda (x) (car x)))
(define arguments-of (lambda (x) (cdr x)))

(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))
(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
         (car (cdr args-in-a-list))))))
(define *const
  ((lambda (:cons :car :cdr :null? :eq? :atom? :zero? :add1 :sub1 :number?)
     (lambda (e table)
       (cond ((number? e) e)
             ((eq? e #t) #t)
             ((eq? e #f) #f)
             ((eq? e (quote cons)) :cons)
             ((eq? e (quote car)) :car)
             ((eq? e (quote cdr)) :cdr)
             ((eq? e (quote null?)) :null?)
             ((eq? e (quote eq?)) :eq?)
             ((eq? e (quote atom?)) :atom?)
             ((eq? e (quote zero?)) :zero?)
             ((eq? e (quote add1)) :add1)
             ((eq? e (quote sub1)) :sub1)
             ((eq? e (quote number?)) :number?))))
   (b-prim cons)
   (a-prim car)
   (a-prim cdr)
   (a-prim null?)
   (b-prim eq?)
   (a-prim atom?)
   (a-prim zero?)
   (a-prim add1)
   (a-prim sub1)
   (a-prim number?)))

(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
          (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond ((number? e) *const)
          ((eq? e #t) *const)
          ((eq? e #f) *const)
          ((eq? e (quote cons)) *const)
          ((eq? e (quote car)) *const)
          ((eq? e (quote cdr)) *const)
          ((eq? e (quote null?)) *const)
          ((eq? e (quote eq?)) *const)
          ((eq? e (quote atom?)) *const)
          ((eq? e (quote zero?)) *const)
          ((eq? e (quote add1)) *const)
          ((eq? e (quote sub1)) *const)
          ((eq? e (quote number?)) *const)
          (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond ((atom? (car e))
           (cond ((eq? (car e) (quote quote)) *quote)
                 ((eq? (car e) (quote lambda)) *lambda)
                 ((eq? (car e) (quote set!)) *set)
                 ((eq? (car e) (quote cond)) *cond)
                 ((eq? (car e) (quote let/cc)) *letcc)
                 (else *application)))
          (else *application))))


;(define the-empty-table
;  (lambda (name)
;    (car (quote ()))))

(define the-empty-table
  (lambda (name)
    (abort2 (cons (quote no-answer) (cons name (quote ()))))))

(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond ((eq? name2 name1) value)
            (else (table name2))))))

;(define test-table the-empty-table)
;(set! test-table (extend 'name 'taro test-table))
;(set! test-table (extend 'nationality 'japan test-table))
;(lookup test-table 'name)
;(lookup test-table 'nationality)
; (lookup test-table 'gender)

;(lambda (name)
;  (car (quote ())))
;(lambda (name2)
;  (cond ((eq? name2 'name) 'taro)
;        (else ((lambda (name)
;                 (car (quote ()))) name2))))
;
;(lambda (name2)
;  (cond ((eq? name2 'nationality) 'japan)
;        (else ((lambda (name2)
;                 (cond ((eq? name2 'name) 'taro)
;                       (else ((lambda (name)
;                                (car (quote ()))) name2)))) name2))))

;((lambda (name2)
;  (cond ((eq? name2 'nationality) 'japan)
;        (else ((lambda (name2)
;                 (cond ((eq? name2 'name) 'taro)
;                       (else ((lambda (name)
;                                (car (quote ()))) name2)))) name2))))
; 'name)
;((lambda (name2)
;  (cond ((eq? name2 'nationality) 'japan)
;        (else ((lambda (name2)
;                 (cond ((eq? name2 'name) 'taro)
;                       (else ((lambda (name)
;                                (car (quote ()))) name2)))) name2))))
; 'nationality)
;((lambda (name2)
;  (cond ((eq? name2 'nationality) 'japan)
;        (else ((lambda (name2)
;                 (cond ((eq? name2 'name) 'taro)
;                       (else ((lambda (name)
;                                (car (quote ()))) name2)))) name2))))
; 'gender)

;(define value
;  (lambda (e)
;    (cond ((define? e) (*define e))
;          (else (the-meaning e)))))

(define abort2 #f)
(define value
  (lambda (e)
    (let/cc the-end
      (set! abort2 the-end)
      (cond ((define? e) (*define e))
            (else (the-meaning e))))))

(define define?
  (lambda (e)
    (cond ((atom? e) #f)
          ((atom? (car e)) (eq? (car e) (quote define)))
          (else #f))))

(define global-table the-empty-table)
(define *define
  (lambda (e)
    (set! global-table
          (extend (name-of e)
                  (box (the-meaning (right-side-of e)))
                  global-table))))

(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new) (set! it new))))))

(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

;(newline)
;(define testbox (box 'a))
;(unbox testbox)
;(setbox testbox 'b)
;(unbox testbox)
;
;(newline)
;(define val 'a)
;val
;(set! val 'b)
;val
;
;(newline)
;(box 'a)
;(define <it> 'a)
;(lambda (sel) (sel <it> (lambda (new) (set! <it> new))))
;
;; (unbox testbox)
;((lambda (sel) (sel <it> (lambda (new) (set! <it> new)))) (lambda (it set) it))
;((lambda (it set) it) <it> (lambda (new) (set! <it> new)))
;<it>
;'a
;
;; (setbox testbox 'b)
;((lambda (sel) (sel <it> (lambda (new) (set! <it> new)))) (lambda (it set) (set 'b)))
;((lambda (it set) (set 'b)) <it> (lambda (new) (set! <it> new)))
;((lambda (new) (set! <it> new)) 'b)
;(set! <it> 'b)
;
;; (unbox testbox)
;((lambda (sel) (sel <it> (lambda (new) (set! <it> new)))) (lambda (it set) it))
;((lambda (it set) it) <it> (lambda (new) (set! <it> new)))
;<it>
;'b

(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *quote
  (lambda (e table)
    (text-of e)))

(newline)
(value '(quote a))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(newline)
(value '(define a (quote aaa)))
(value 'a)

(define *set
  (lambda (e table)
    (setbox (lookup table (name-of e))
            (meaning (right-side-of e) table))))

(newline)
(value '(set! a (quote bbb)))
(value 'a)

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend (formals-of e)
                            (box-all args)
                            table)))))

(define beglis
  (lambda (es table)
    (cond ((null? (cdr es)) (meaning (car es) table))
          (else ((lambda (val) (beglis (cdr es) table))
                 (meaning (car es) table))))))

(define box-all
  (lambda (vals)
    (cond ((null? vals) (quote ()))
          (else (cons (box (car vals)) (box-all (cdr vals)))))))

(define multi-extend
  (lambda (names values table)
    (cond ((null? names) table)
          (else (extend (car names)
                        (car values)
                        (multi-extend (cdr names) (cdr values) table))))))

(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define evlis
  (lambda (args table)
    (cond ((null? args) (quote ()))
          (else ((lambda (val)
                   (cons val (evlis (cdr args) table)))
                 (meaning (car args) table))))))

(newline)
(value '(lambda (x) x))
(value '((lambda (x) x) 1))
(value '(define id (lambda (x) x)))
(value '(id 2))

(newline)
(value '(define otrue (lambda (x y) x)))
(value '(define ofalse (lambda (x y) y)))
(value '(define oif (lambda (cnd thn els) (cnd thn els))))
(value '(oif otrue 1 2))
(value '(oif ofalse 1 2))

;(value '(lambda (x y) (set! a x) y))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evcon
  (lambda (lines table)
    (cond ((else? (question-of (car lines)))
           (meaning (answer-of (car lines)) table))
          ((meaning (question-of (car lines)) table)
           (meaning (answer-of (car lines)) table))
          (else (evcon (cdr lines) table)))))

(newline)
(value '(cond (else 0)))
(value '(cond ((null? (cons 0 (quote ()))) 0) (else 1)))
;(value '(cond))

(define *letcc
  (lambda (e table)
    (let/cc skip
      (beglis (ccbody-of e)
              (extend (name-of e)
                      (box (a-prim skip))
                      table)))))

(newline)
(value '(let/cc hop 1))
(value '(let/cc hop (hop 1) 2))

(newline)

(the-empty-table 'z)

(value 'z)
