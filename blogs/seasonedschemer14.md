# Scheme修行(14) アレふたたび

突然話題が変わります

> lengthを覚えていますか。

さすがに見ないで書けるようになりました

> 次の値は何ですか。
>
> ```
> (define length
>   (lambda (l) 0))
>
> (set! length
>       (lambda (l)
>         (cond ((null? l) 0)
>               (else (add1 (length (cdr l)))))))
> ```

値は例によってありませんが、何かへんてこなことを始めました
このlengthでも動くことは動きます
次に第16の戒律に沿って書き換えます

```
(define length
  (let ((h (lambda (l) 0)))
    (set! h (lambda (l)
              (cond ((null? l) 0)
                    (else (add1 (h (cdr l)))))))
    h))
```

何でしょうかこれは  
今までとは違うletの使い方が加わった模様

> 第17の戒律（最終版）  
> (let ((x ...)) ...)に対して(set! x ...)を用いる際には、
> それらの間に少なくとも1つの(lambda ... を置くか、
> xの新しい値がxを参照する関数のときのみにせよ。

(add1 (h (cdr l)))の方に出てくるhは、元のhを指してるのか  
いま定義してる真っ最中のhを指してるのか  
書いてあることからはわかりませんが  
このlengthが普通のlengthと同じように動くためには  
hが自分自身を参照している必要があります

lengthっぽい部分を取り出してLとします

```
(define length
  (let ((h (lambda (l) 0)))
    (set! h (L (lambda (arg) (h arg))))
    h))

(define L
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
```

(lambda (arg) (h arg))ってどこかで見たような  
これはもしや・・・アレでしょうか

ここらへんで一度動きを確かめてみます

```
(length '(a b))
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
```

確かに動きます  
SICPには代入が出てくるとこういうやりかたはうまくいかないって書いてありましたが

h のことをわざわざ (lambda (arg) (h arg)) と書いているのは  
きっとScheme手習いでやったように無限ループになるんだろうな  

試しにそのままhと書いてみます

```
(define length
  (let ((h (lambda (l) 0)))
    (set! h (L h))
    h))

> (length '(a b c))
1
```

ええ？こうじゃないの？しかも1って何

```
(length '(a b))
(h '(a b))
((L h) '(a b))
(((lambda (length) ...) (L h)) '(a b))
(((lambda (length) ...) ((lambda (length) ...) (L h))) '(a b))
(((lambda (length) ...) ((lambda (length) ...) ((lambda (length) ...) (L h)))) '(a b))
```

...  
考えてもわからなかったのでスルーします  
心苦しいけれども本筋とは関係ないので  
説明できる方いたらぜひ教えて下さい

> lengthがLの関数になるように、定義を書きなおしてください。
> その新しい関数をY!とします。

やっぱりYのしわざか

```
(define Y!
  (lambda (L)
    (let ((h (lambda (l) (quote ()))))
      (set! h (L (lambda (arg) (h arg))))
      h)))

(define length (Y! L))
```

こうも書けます

```
(define Y-bang
  (lambda (f)
    (letrec ((h (f (lambda (arg) (h arg)))))
      h)))

(define length (Y-bang L))
```

letrecはletとsetがあれば作れるということ  
シンタックスシュガーというわけです  

振り返るとScheme手習いの時に無理やり作ったdefineはdefineというより  
劣化版letrecだった気がします

> ここで見てきたことは、まさに「適用順手続き的Yコンビネータ」の導出です。

そういうわけでした

Scheme手習いでやった「適用順Yコンビネータ」はコード見ても何がなんだかさっぱりでした  
こちらは細かい動きはともかくとして再帰させようとしてるんだということくらいはわかります

> (Y f)の値は(Y! f)の値と同じ再帰関数だというのは正しいですか。
> はい、関数Y!はこの形をしたすべてのfに対してYと同じ再帰関数を作り出します。

正しいそうですが同じって何でしょう  
実質同じ働きをする関数という意味ではそのとおりだと思いますが  
fとしてLやDを与えてみても字面まで同じになるわけではなさそうです

Yに食わせる関数とは異なる形のこんな関数で試します  
bizはbizarreの略です

```
(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a) (if (= a x) 0 (f a))))))
```

字面的には、再帰するたびにxがひとつずつ増えていって5になったところで0を返しそうな雰囲気です  

((Y biz) 5)は想定どおり0を返しますが、((Y! biz) 5)は無限ループになってしまいます  
xが1のまま増えていってないようです  
(set! x (add1 x))が1回しか評価されてないのか、毎回別のxが作られて0になっているのか、  
そんなところでしょうか  

細かく追ってみます  
((Y! biz) 5)の評価ではxが1、fが(lambda (arg) (h arg))、  
関数本体が(lambda (a) (if (= a x) 0 (f a)))なクロージャがhに結び付けられてから  
5にhが適用されるぽいです  
(set! x (add1 x))はすでに評価済みのため、その後xが加算されることはありません

いつもどおりに式を並べていく方式だと途中で破綻してしまうので  
evalの気持ちになって考えてみました  
こんな感じです

1. (define biz ...) を評価します
1. xが0、関数本体が (lambda (f) ...)なクロージャにbizが結び付けられます
1. ((Y! biz) 5)を評価します
1. (Y! biz)を評価します
1. Y!は(lambda (L) ...)に評価されます
1. bizはxが0、関数本体が (lambda (f) ...)なクロージャに評価されます
1. bizにY!を適用します
1. Lにxが0、関数本体が (lambda (f) ...)なクロージャが結び付けられます
1. Y!でhをいったん作ります
1. Lに(lambda (arg) (h arg))を適用します
1. fに(lambda (arg) (h arg))が結び付けられます
1. xに1が加えられます
1. xが1、fが(lambda (arg) (h arg))、関数本体が(lambda (a) (if (= a x) 0 (f a)))なクロージャがhに結び付けられます
1. 5にhを適用します

一方((Y biz) 5)ではbizのlambdaがそのままの形で再帰されるため  
呼び出しのたびに(set! x (add1 x))が評価され、想定どおりにxが加算されていきます

> 帽子はまだ小さくなっていませんか。

Yコンビネータがわかっているならこれくらいでは小さくならないそうですが  
けっこう小さくなりましたよ
