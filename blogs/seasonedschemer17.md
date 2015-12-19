# Scheme修行 (17) 2つの「同じ」
前回のwrideはちょっと手を抜きすぎて結果が分かりづらかったのでやっぱりもうちょっとそれっぽく表示してくれるようにします

```
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
```

こうなります

```
> (wride (kons 1 (kons (kons 2 (kons 3 '())) '())))
(1 (2 3))

> (wride (kons (kons 1 (kons 2 '())) (kons 3 '())))
((1 2) 3)
```

いちおうこういう表示にも対応したという自己満足

```
> (wride (kons (kons 'a 'b) 'c))
((a . b) . c)

> (wride (kons 'a (kons 'b 'c)))
(a b . c)
```

さて本題

```
(define lots
  (lambda (m)
    (cond ((zero? m) (quote ()))
          (else (kons (quote egg) (lots (sub1 m)))))))

(define add-at-end
  (lambda (l)
    (cond ((null? (kdr l))
           (konsC (kar l) (kons (quote egg) (quote ()))))
          (else
           (konsC (kar l) (add-at-end (kdr l)))))))
```

konsCはconsCと同じく、呼びだされた回数を覚えているkonsです
なぜ()とkonsするところはkonsCじゃないんでしょうね？
リストの長さと関係ないから？

```
> (wride (add-at-end (lots 3)))
(egg egg egg egg)

> (kounter)
3
```

add-at-endではkonsCを3回実行していることがわかります
まるごとリストを作りなおしてますからね

> 最後のkons以外には新しいkonsをせず、末尾に卵を追加することはできますか。

set-kdrを使います

```
(define add-at-end-too
  (lambda (l)
    (letrec ((A (lambda (l)
                  (cond ((null? (kdr l))
                         (set-kdr l (kons (quote egg) (quote ()))))
                        (else
                         (A (kdr l)))))))
      (A l)
      l)))
```

結果は同じです

```
> (set-kounter 0)
> (wride (add-at-end-too (lots 3)))
(egg egg egg egg)
```

当然すぎるほど当然ですがkonsCは呼び出されていません

```
> (kounter)
0
```

immutableな世界からmutableな世界に入りました
すると今までは起こらなかったようなことが起こります
konsの回数を数えてみます
（ここでは掲載のソースのkonsとkonsCを両方数えているようです）

```
(define dozen (lots 12)) ; 12回
(define bakers-dozen (add-at-end dozen)) ; 13回
(define bakers-dozen-too (add-at-end-too dozen)) ; 1回
(define bakers-dozen-again (add-at-end dozen)) ; 14回(!)
```

2行目と4行目は同じことをしているはずですがkonsの回数が違っています
本では12+13+1が27になってますがご愛嬌 この本はけっこう誤植多いです
3行目のadd-at-end-tooで、大元のdozenにひとつeggがくっついてしまったからです
くわしく見てみます

(define dozen (lots 12))を実行すると卵が12個できます

```
dozen
  ↓
 egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->()
```

(define bakers-dozen (add-at-end dozen))はdozenのコピーを作ってから
卵をひとつ追加します
コピーなので元のdozenには影響は与えません

```
dozen
  ↓
 egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->()

bakers-dozen
  ↓
 egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->()
```

(define bakers-dozen-too (add-at-end-too dozen))はdozenを末尾まで
たどってそこに卵を追加します
bakers-dozen-tooはもちろん13個の卵を指しますが、元のdozenまで13個の卵を
指すようになってしまいます

```
dozen
  ↓
 egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->()
  ↑
bakers-dozen-too

bakers-dozen
  ↓
 egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->()
```

(define bakers-dozen-again (add-at-end dozen))は、13個になった
dozenをコピーしてから末尾に卵を追加するのでkonsCが14回呼ばれることになります

```
dozen
  ↓
 egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->()
  ↑
bakers-dozen-too

bakers-dozen
  ↓
 egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->()

bakers-dozen-again
  ↓
 egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->()
```

さて

> (set! ...)を導入すると、「同じこと」について新しい考え方ができます。

dozenとbakers-dozenはどちらも13個の卵を指しています
set!を使わないかぎり、dozenとbakers-dozenを区別する必要はありませんでした

> 2つのkonsは、片方を変えるともう片方も変わるなら、同じものです。

アドレスやポインタを前提にして話すのではないとするとこういう言い方になるんでしょうね
dozenとbakers-dozen-tooは全く同じものを指しています
bakers-dozen-tooを変えるとdozenも変わってしまいます

そのまま実装した「同じ」判定器です
実用上の価値はほとんどないと思います

```
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
```

本には(same? bakers-dozen bakers-dozen-too)が#tと書いてありますが

```
> (same? dozen bakers-dozen-too)
#t

> (same? bakers-dozen bakers-dozen-too)
#f
```

そこは間違っちゃいかんとこでしょ・・・