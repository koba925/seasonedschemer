#lang racket

(require rackunit)
(require "util.rkt")

(define text-of (lambda (x) (car (cdr x))))
(define formals-of (lambda (x) (car (cdr x))))
(define body-of (lambda (x) (cdr (cdr x))))
(define binds-of (lambda (x) (car (cdr x))))
(define letbody-of (lambda (x) (cdr (cdr x))))
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
                 ((eq? (car e) (quote let)) *let)
                 ((eq? (car e) (quote letrec)) *letrec)
                 ((eq? (car e) (quote set!)) *set)
                 ((eq? (car e) (quote cond)) *cond)
                 ((eq? (car e) (quote let/cc)) *letcc)
                 ((macro? (car e)) *macro-application) ; ここ追加
                 (else *application)))
          (else *application))))

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

(define abort2 #f)
;(define value
;  (lambda (e)
;    (let/cc the-end
;      (set! abort2 the-end)
;      (cond ((define? e) (*define e))
;            ((defmac? e) (*defmac e)) ; ここ追加
;            (else (the-meaning e))))))

(define value
  (lambda (e)
    (let/cc the-end
      (set! abort2 the-end)
      (the-meaning e))))

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

(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

;(define meaning
;  (lambda (e table)
;    ((expression-to-action e) e table)))
(define meaning
  (lambda (e table)
    (cond ((define? e) (*define e))
          ((defmac? e) (*defmac e))
          (else ((expression-to-action e) e table)))))


(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (let ((m (lookup macro-table e))) ;ここ
      (cond ((eq? m #f) (unbox (lookup table e))) ;ここ
            (else m))))) ;ここ

(define *set
  (lambda (e table)
    (setbox (lookup table (name-of e))
            (meaning (right-side-of e) table))))

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend (formals-of e)
                            (box-all args)
                            table)))))

;(define *lambda
;  (lambda (e table)
;    (lambda (selector)
;      (selector (quote lambda)
;                (lambda (e table)
;                  (lambda (args)
;                    (beglis (body-of e)
;                            (multi-extend (formals-of e)
;                                          (box-all args)
;                                          table))))))))

;(define closure-kind
;  (lambda (closure)
;    (closure (lambda (kind proc) kind))))
;(define closure-proc
;  (lambda (closure)
;    (closure (lambda (kind proc) proc))))
;

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

(define *letcc
  (lambda (e table)
    (let/cc skip
      (beglis (ccbody-of e)
              (extend (name-of e)
                      (box (a-prim skip))
                      table)))))


(define let-formals-of
  (lambda (binds)
    (cond ((null? binds) (quote ()))
          (else (cons (car (car binds)) (let-formals-of (cdr binds)))))))
(define let-args-of
  (lambda (binds)
    (cond ((null? binds) (quote ()))
          (else (cons (car (cdr (car binds))) (let-args-of (cdr binds)))))))
;(define *let
;  (lambda (e table)
;    ((lambda (args)
;       (beglis (letbody-of e)
;               (multi-extend (let-formals-of (binds-of e))
;                             (box-all args)
;                             table)))
;     (evlis (let-args-of (binds-of e)) table))))

(define let2lambda
  (lambda (e)
    (cons (cons (quote lambda)
                (cons (let-formals-of (binds-of e))
                      (letbody-of e)))
          (let-args-of (binds-of e)))))
(define *let
  (lambda (e table)
    (meaning (let2lambda e) table)))

(define temp-symbol
  (lambda (sym)
    (string->symbol (string-append "$$" (symbol->string sym)))))
(define letrec-formals
  (lambda (binds)
    (cond ((null? binds) (quote ()))
          (else (let ((fml (car (car binds))))
                  (cons (cons fml (cons 0 (quote ())))
                        (letrec-formals (cdr binds))))))))
(define letrec-vals
  (lambda (binds)
      (cond ((null? binds) (quote ()))
            (else (let ((fml (car (car binds)))
                        (val (car (cdr (car binds)))))
                    (cons (cons (temp-symbol fml)
                                (cons val (quote ())))
                          (letrec-vals (cdr binds))))))))
(define letrec-sets
  (lambda (binds)
      (cond ((null? binds) (quote ()))
            (else (let ((fml (car (car binds))))
                    (cons (cons (quote set!)
                                (cons fml
                                      (cons (temp-symbol fml)
                                            (quote ()))))
                          (letrec-sets (cdr binds))))))))
(define letrec2let
  (lambda (e)
    (let ((binds (binds-of e)))
      (cons (quote let)
            (cons (letrec-formals binds)
                  (cons (cons (quote let)
                              (cons (letrec-vals binds)
                                    (letrec-sets binds)))
                        (letbody-of e)))))))
(define *letrec
  (lambda (e table)
    (meaning (letrec2let e) table)))

;まずは真似して書いてみる
(define defmac?
  (lambda (e)
    (cond ((atom? e) #f)
          ((atom? (car e)) (eq? (car e) (quote defmac)))
          (else #f))))

;マクロとlambdaを区別する手段としてテーブルを分ける

;マクロ用テーブル
;見つからなければ#fを返す
;見つかればboxが返されるから区別はつく
(define macro-table (lambda (name) #f))

;マクロだけどlambdaで書く（しかない？）
;set!はしないことにしてboxは使わない
(define *defmac
  (lambda (e)
    (set! macro-table
          (extend (name-of e)
                  (the-meaning (right-side-of e))
                  macro-table))))

;マクロかどうかの判別
(define macro?
  (lambda (e)
    (lookup macro-table e)))

;適用
(define *macro-application
  (lambda (e table)
;    (value (expand e table))))
    (meaning (expand e table) table)))
(define expand
  (lambda (e table)
    ((meaning (function-of e) table)
              (arguments-of e))))

(value '(let ((x (quote a)) (y (cons (quote b) (quote ())))) (cons x y)))

(letrec2let
 '(letrec ((mr (lambda (lat)
                 (cond ((null? lat) (quote ()))
                       ((eq? a (car lat)) (mr (cdr lat)))
                       (else (cons (car lat) (mr (cdr lat))))))))
    (mr lat)))
(let2lambda
 (letrec2let
  '(letrec ((mr (lambda (lat)
                  (cond ((null? lat) (quote ()))
                        ((eq? a (car lat)) (mr (cdr lat)))
                        (else (cons (car lat) (mr (cdr lat))))))))
     (mr lat))))

(value '(define multirember
          (lambda (a lat)
            (letrec ((mr (lambda (lat)
                           (cond ((null? lat) (quote ()))
                                 ((eq? a (car lat)) (mr (cdr lat)))
                                 (else (cons (car lat) (mr (cdr lat))))))))
              (mr lat)))))
(value '(multirember (quote a) (quote (a b a c))))

(value '(defmac set1
          (lambda (name)
            (cons (quote set!)
                  (cons name
                        (cons 1 (quote ())))))))
(expand '(set1 a) lookup-in-global-table)
(value '(define a 0))
(value 'a)
(value '(set1 a))
(value 'a)

(value '(define let-formals-of
          (lambda (binds)
            (cond ((null? binds) (quote ()))
                  (else (cons (car (car binds)) (let-formals-of (cdr binds))))))))
(value '(define let-args-of
          (lambda (binds)
            (cond ((null? binds) (quote ()))
                  (else (cons (car (cdr (car binds))) (let-args-of (cdr binds))))))))
(value '(defmac my-let
          (lambda (binds body)
            (cons (cons (quote lambda)
                        (cons (let-formals-of binds) body))
                  (let-args-of binds)))))
(expand '(my-let
           ((x (quote a))
            (y (cons (quote b) (quote ()))))
           ((cons x y)))
        lookup-in-global-table)
(value '(my-let
          ((x (quote a))
           (y (cons (quote b) (quote ()))))
          ((cons x y))))

(value '(defmac my-define
          (lambda (form body)
            (cons 'define
                  (cons (car form)
                        (cons (cons 'lambda
                                    (cons (cdr form)
                                          (cons body (quote ()))))
                              (quote ())))))))

(expand '(my-define (add2 x) (add1 (add1 x))) lookup-in-global-table)
(value '(my-define (add2 x) (add1 (add1 x))))
(value '(add2 1))

(value '(defmac make-pair
          (lambda (a b)
            (cons 'cons
                  (cons a
                        (cons (cons 'cons
                                    (cons b
                                          (cons (cons (quote quote)
                                                      (cons (quote ()) (quote ())))
                                                (quote ()))
                                          (quote ())))
                              (quote ())))))))
       
(expand '(make-pair (quote aaa) (quote bbb)) lookup-in-global-table)
(value '(make-pair (quote aaa) (quote bbb)))

(value '((lambda (x) ((lambda (y) (eq? x y)) 1)) 1))

(expand '(my-let ((x (quote a)))
                 ((my-let ((y (cons (quote b) (quote ()))))
                          ((cons x y)))))
        lookup-in-global-table)

(value '(my-let ((x (quote a)))
                ((my-let ((y (cons (quote b) (quote ()))))
                          ((cons x y))))))

(value '((lambda (x)
           (define temp 3)
           (eq? temp x))
         4))

(value 'temp)


