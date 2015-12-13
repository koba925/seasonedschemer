# Scheme修行 (16) consをlambdaで表現する

18章「我変わる、ゆえに我同じなり！」のひとつのヤマはこれ

```
(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

(define kar
  (lambda (c)
    (c (lambda (a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (a d) d))))
```

cons、car、cdrをlambdaで書いてしまったというもの
Schemeのコア中のコアと思っていたものが、実はlambdaで書けるという
楽しくなってまいりました（個人的趣味

konsの値は表示してくれませんが

```
> (kons 2 (kons 1 (quote ())))
#<procedure:...hemer/chap18.rkt:7:4>
```

karやkdrで皮をめくっていけばアトムは表示されます

```
> (kar (kons 2 (kons 1 (quote ())))) ;(car '(2 1))
2

> (kar (kdr (kons 2 (kons 1 (quote ()))))) ;(car (cdr '(2 1)))
1

> (kdr (kdr (kons 2 (kons 1 (quote ())))))  ;(cdr (cdr '(2 1)))
'()
```

konsにselectorという関数を渡しているのが目に新鮮です
やってることはそれほど難しくありません
konsがクロージャにkarとkdrの値を保存し、
karはkarを返す関数を、kdrはkdrを返す関数をkonsに渡してやっているだけです

(kons 1 (quote ()) はこんなクロージャを返し、

- kar → 1
- kdr → '()
- 仮引数 → (selector)
- 関数本体 → (selector car cdr)

(kons 2 (kons 1 (quote ()))はこんなクロージャを返す、というわけです

- kar → 2
- kdr → こんなクロージャ
  - kar → 1
  - kdr → '()
  - 仮引数 → (selector)
  - 関数本体 → (selector kar kdr)
- 仮引数 → (selector)
- 関数本体 → (selector kar kdr)

ポインタ的な実装と同じように動いてくれそうです
考えるときは箱と矢印を使った表現でよさそうです
そうじゃないと困りますし

> konsはconsの影法師ですか。
> 
> そうです。
> 
> konsはconsと違っていますか。
> 
> 確かに違いますが、6章(「Scheme手習い」)で「影法師に注意」と言ったことを
> 忘れないで下さい。

なぜ逆接でつながっているのかよくわかりませんが、忘れないようにしましょう

次に、kdrを書き換えられるようにします
karを書き換えられるようにしないのは紙面の都合ってやつでしょうか
書き換えられなくっていいということはなさそうですし

```
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
```

konsがどうなっているのか見えないのも不便ですのでkonsを目に見えるように
表示するような関数でも作りましょう

```
(define wride
  (lambda (l)
    (letrec ((W (lambda (l)
                  (cond ((null? l) (display "'()"))
                        ((atom? l) (write l))
                        (else
                         (display "(kons ")
                         (W (kar l))
                         (display " ")
                         (W (kdr l))
                         (display ")"))))))
      (W l)
      (newline))))
```

どれどれ

```
> (wride (kons 1 '()))
#<procedure:...hemer/chap18.rkt:8:6>
```

ぶほ

・・・

konsはlambdaですがlambdaはatom?ですのでリストとは思ってもらえませんでした
まんまと影法師にしてやられています

考えてもkonsが作ったlambdaと他のlambdaを区別する方法が思いつかなかったので
lambdaはアトムってことにしました
ほんとは無茶ですが

```
(define adom?
  (lambda (s)
    (and (atom? s) (not (procedure? s)))))

(define wride
  (lambda (l)
    (letrec ((W (lambda (l)
                  (cond ((null? l) (display "'()"))
                        ((adom? l) (write l))
                        (else
                         (display "(kons ")
                         (W (kar l))
                         (display " ")
                         (W (kdr l))
                         (display ")"))))))
      (W l)
      (newline))))
```

どうかな

```
> (wride (kons 1 (kons (kons 2 (kons 3 '())) '())))
(kons 1 (kons (kons 2 (kons 3 '())) '()))
```

大丈夫そうです

これで新しいkonsとset-kdrのテストがしやすくなります

```
> (define l (kons 1 (kons 2 '())))
> (wride l)
(kons 1 (kons 2 '()))

> (set-kdr l (kons 2 (kons 3 '())))
> (wride l)
(kons 1 (kons 2 (kons 3 '())))

> (wride (kdr l))
(kons 2 (kons 3 '()))

> (wride (kar (kdr l)))
2
```

おｋぽいです

ところでkonsを作るのになぜいったんbonsを作っているのでしょう
いきなりkonsだって作れそうですが

```
(define gons
  (lambda (kar kdr)
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr))))
```

これでも普通に動くんですけどねえ
より小さい単位に分割したってことでしょうか
