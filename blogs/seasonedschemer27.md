
# Scheme修行 (27) let、letrec
私の空気読みによると、なんかletやletrecは宿題ね、と言われているような気がする
lambdaができてるからきっと簡単にできるはず

えーとlambdaしてapplicationするということだから

```
(define binds-of (lambda (x) (car (cdr x))))
(define letbody-of (lambda (x) (cdr (cdr x))))

(define list-to-action
  (lambda (e)
    (cond ((atom? (car e))
           (cond ...
                 ((eq? (car e) (quote let)) *let)
                 ...
                 (else *application)))
          (else *application))))

(define let-formals-of
  (lambda (binds)
    (cond ((null? binds) (quote ()))
          (else (cons (car (car binds)) (let-formals-of (cdr binds)))))))
(define let-args-of
  (lambda (binds)
    (cond ((null? binds) (quote ()))
          (else (cons (car (cdr (car binds))) (let-args-of (cdr binds)))))))
(define *let
  (lambda (e table)
    ((lambda (args)
       (beglis (letbody-of e)
               (multi-extend (let-formals-of (binds-of e))
                             (box-all args)
                             table)))
```

こうかな

```
> (value '(let ((x (quote a)) (y (cons (quote b) (quote ())))) (cons x y)))
'(a b)
```

よし

こうじゃなくて、いったんeをlambdaに書き換えてやってからmeaningにかける

```
(let ((x a) (y b)) z1 z2)
↓
((lambda (x y) z1 z2) a b)
```

となるように書き換えるわけだから

```
(define let2lambda
  (lambda (e)
    (cons (cons (quote lambda)
                (cons (let-formals-of (binds-of e))
                      (letbody-of e)))
          (let-args-of (binds-of e)))))
(define *let
  (lambda (e table)
    (meaning (let2lambda e) table)))
```

ですかね
consで目が回りそうですが

```
> (value '(let ((x (quote a)) (y (cons (quote b) (quote ())))) (cons x y)))
'(a b)
```

うまくいきました

letrecはもうちょっとややこしかったですね

```
(letrec ((x1 a1) (x2 a2)) z1 z2)
=
((let ((x1 0) (x2 0)) 
   (let ((y1 a1) (y2 a2))
     (set! x1 y1) (set! x2 y2) z1 z2))
```

でしたから

えーと
あっ
y1とかy2とかが作れない

シンボル名をいじるどころか文字列も扱えないんだった
どうしよう
そこだけ文字列型使うか
セルフで実行できなくなるけど

```
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
```

なんか長いなあ
consがもうわけわからんし
でも動くことは動く

```
> (value '(define multirember
            (lambda (a lat)
              (letrec ((mr (lambda (lat)
                             (cond ((null? lat) (quote ()))
                                   ((eq? a (car lat)) (mr (cdr lat)))
                                   (else (cons (car lat) (mr (cdr lat))))))))
                (mr lat)))))
> (value '(multirember (quote a) (quote (a b a c))))
'(b c)
```

ここまでやったからには簡単でいいからマクロとして実装してみたいですね
以下次号
