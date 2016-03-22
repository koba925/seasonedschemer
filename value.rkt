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

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(value '(define text-of (lambda (x) (car (cdr x)))))
(value '(define formals-of (lambda (x) (car (cdr x)))))
(value '(define body-of (lambda (x) (cdr (cdr x)))))
(value '(define ccbody-of (lambda (x) (cdr (cdr x)))))
(value '(define name-of (lambda (x) (car (cdr x)))))
(value '(define right-side-of
          (lambda (x)
            (cond ((null? (cdr (cdr x))) 0)
                  (else (car (cdr (cdr x))))))))
(value '(define cond-lines-of (lambda (x) (cdr x))))
(value '(define else?
          (lambda (x)
            (cond ((atom? x) (eq? x (quote else)))
                  (else #f)))))
(value '(define question-of (lambda (x) (car x))))
(value '(define answer-of (lambda (x) (car (cdr x)))))
(value '(define function-of (lambda (x) (car x))))
(value '(define arguments-of (lambda (x) (cdr x))))

(value '(define a-prim
          (lambda (p)
            (lambda (args-in-a-list)
              (p (car args-in-a-list))))))
(value '(define b-prim
          (lambda (p)
            (lambda (args-in-a-list)
              (p (car args-in-a-list)
                 (car (cdr args-in-a-list)))))))
(value '(define *const
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
           (a-prim number?))))

(value '(define expression-to-action
          (lambda (e)
            (cond ((atom? e) (atom-to-action e))
                  (else (list-to-action e))))))

(value '(define atom-to-action
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
                  (else *identifier)))))

(value '(define list-to-action
          (lambda (e)
            (cond ((atom? (car e))
                   (cond ((eq? (car e) (quote quote)) *quote)
                         ((eq? (car e) (quote lambda)) *lambda)
                         ((eq? (car e) (quote set!)) *set)
                         ((eq? (car e) (quote cond)) *cond)
                         ((eq? (car e) (quote let/cc)) *letcc)
                         (else *application)))
                  (else *application)))))

(value '(define the-empty-table
          (lambda (name)
            (abort2 (cons (quote no-answer) (cons name (quote ())))))))

(value '(define lookup
          (lambda (table name)
            (table name))))

(value '(define extend
          (lambda (name1 value table)
            (lambda (name2)
              (cond ((eq? name2 name1) value)
                    (else (table name2)))))))

(value '(define abort2 #f))
(value '(define value
          (lambda (e)
            (let/cc the-end
              (set! abort2 the-end)
              (cond ((define? e) (*define e))
                    (else (the-meaning e)))))))

(value '(define define?
          (lambda (e)
            (cond ((atom? e) #f)
                  ((atom? (car e)) (eq? (car e) (quote define)))
                  (else #f)))))

(value '(define global-table the-empty-table))
(value '(define *define
          (lambda (e)
            (set! global-table
                  (extend (name-of e)
                          (box (the-meaning (right-side-of e)))
                          global-table)))))

(value '(define box
          (lambda (it)
            (lambda (sel)
              (sel it (lambda (new) (set! it new)))))))

(value '(define setbox
          (lambda (box new)
            (box (lambda (it set) (set new))))))

(value '(define unbox
          (lambda (box)
            (box (lambda (it set) it)))))

(value '(define the-meaning
          (lambda (e)
            (meaning e lookup-in-global-table))))

(value '(define lookup-in-global-table
          (lambda (name)
            (lookup global-table name))))

(value '(define meaning
          (lambda (e table)
            ((expression-to-action e) e table))))

(value '(define *quote
          (lambda (e table)
            (text-of e))))

(value '(define *identifier
          (lambda (e table)
            (unbox (lookup table e)))))

(value '(define *set
          (lambda (e table)
            (setbox (lookup table (name-of e))
                    (meaning (right-side-of e) table)))))

(value '(define *lambda
          (lambda (e table)
            (lambda (args)
              (beglis (body-of e)
                      (multi-extend (formals-of e)
                                    (box-all args)
                                    table))))))

(value '(define beglis
          (lambda (es table)
            (cond ((null? (cdr es)) (meaning (car es) table))
                  (else ((lambda (val) (beglis (cdr es) table))
                         (meaning (car es) table)))))))

(value '(define box-all
          (lambda (vals)
            (cond ((null? vals) (quote ()))
                  (else (cons (box (car vals)) (box-all (cdr vals))))))))

(value '(define multi-extend
          (lambda (names values table)
            (cond ((null? names) table)
                  (else (extend (car names)
                                (car values)
                                (multi-extend (cdr names) (cdr values) table)))))))

(value '(define *application
          (lambda (e table)
            ((meaning (function-of e) table)
             (evlis (arguments-of e) table)))))

(value '(define evlis
          (lambda (args table)
            (cond ((null? args) (quote ()))
                  (else ((lambda (val)
                           (cons val (evlis (cdr args) table)))
                         (meaning (car args) table)))))))

(value '(define *cond
          (lambda (e table)
            (evcon (cond-lines-of e) table))))

(value '(define evcon
          (lambda (lines table)
            (cond ((else? (question-of (car lines)))
                   (meaning (answer-of (car lines)) table))
                  ((meaning (question-of (car lines)) table)
                   (meaning (answer-of (car lines)) table))
                  (else (evcon (cdr lines) table))))))

(value '(define *letcc
          (lambda (e table)
            (let/cc skip
              (beglis (ccbody-of e)
                      (extend (name-of e)
                              (box (a-prim skip))
                              table))))))

(value '(value (quote
                (define length
                  (lambda (lat)
                    (cond ((null? lat) 0)
                          (else (add1 (length (cdr lat))))))))))
(value '(value (quote
                (length (quote (a b c))))))



