# Scheme修行(5) 第12章 残り & 第13の戒律

## union

`union`をいじります。まずは原型。

```
(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          ((eq? (car lat) a) #t)
          (else (member? a (cdr lat))))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2) (union (cdr set1) set2))
          (else (cons (car set1) (union (cdr set1) set2))))))
```

繰り返しの間`set2`は変化しないので`letrec`に入れます。

```
(define union-r
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond ((null? set) set2)
                    ((member? (car set) set2) (U (cdr set)))
                    (else (cons (car set) (U (cdr set))))))))
      (U set1))))
```

もしかして`member?`を誰かが変にいじってしまっても動くように、自前で`member?`を持つことにします。そんなの気にしないとか言わない。

```
(define union-m
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond ((null? set) set2)
                    ((M? (car set) set2) (U (cdr set)))
                    (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (cond ((null? lat) #f)
                     ((eq? (car lat) a) #t)
                     (else (M? a (cdr lat)))))))
      (U set1))))
```

これで終わりではありません。
`M?`が変化しない引数を持っているので、`letrec`に入れてやります。

```
(define union-m2
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond ((null? set) set2)
                    ((M? (car set) set2) (U (cdr set)))
                    (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond ((null? lat) #f)
                                ((eq? (car lat) a) #t)
                                (else (N? (cdr lat)))))))
                 (N? lat)))))
      (U set1))))
```

ここまでやるかなあ？という気もしますが・・・
読みやすさという点では原型の方が上な気がします。
チームで書くなら原型くらいにしておくかな、という気分。
戒律が共有できていればいいですけど。

## 第13の戒律

> 関数を隠し、守るには、`(letrec ...)`を用いるべし。

その関数からしか呼ばれない関数は隠すようにします。
`member?`みたいなのは標準のライブラリ関数みたいなものなので別でしょうね。

## two-in-a-row?

さっきまでの調子で`letrec`を使うとこのようになります。

```
(define two-in-a-row-r?
  (lambda (lat)
    (letrec
        ((W (lambda (a lat)
              (cond ((null? lat) #f)
                    (else (or (eq? (car lat) a)
                              (W (car lat) (cdr lat))))))))
      (cond ((null? lat) #f)
            (else (W (car lat) (cdr lat)))))))
```

しかし、よく見ると`two-in-a-row-r?`の変数を`W`で共有する必要はありません。
ということでこうも書けます。

```
(define two-in-a-row-r2?
  (letrec
      ((W (lambda (a lat)
            (cond ((null? lat) #f)
                  (else (or (eq? (car lat) a)
                            (W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
            (else (W (car lat) (cdr lat)))))))
```

不必要に変数が見えないので、こっちのほうがよりよい書き方でしょう。

## 練習

もうあとは練習です。`sum-of-prefix`はこうなります。

```
(define sum-of-prefixes-r
  (letrec
      ((S (lambda (sss tup)
            (cond ((null? tup) (quote ()))
                  (else (cons (+ sss (car tup))
                              (S (+ sss (car tup)) (cdr tup))))))))
    (lambda (tup)
      (S 0 tup))))
```

`scramble`はこう。

```
(define scramble-r
  (letrec
      ((P (lambda (tup rp)
            (cond ((null? tup) (quote ()))
                  (else (cons (pick (car tup)
                                    (cons (car tup) rp))
                              (P (cdr tup)
                                 (cons (car tup) rp))))))))
    (lambda (tup)
      (P tup (quote ())))))
```

## 12章のまとめ

* `letrec`でローカルに関数を定義し、外部から隠すことを覚えました。
* 繰り返しの間変わらない変数はローカルな関数を定義して渡さないようにすることを覚えました。
* 「避難しましょう」って何？