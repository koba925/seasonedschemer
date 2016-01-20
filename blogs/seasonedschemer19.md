# Scheme修行 (19) 継続と収集子

deepです

```
(define deep
  (lambda (m)
    (cond ((zero? m) (quote pizza))
          (else (cons (deep (sub1 m)) (quote ()))))))
```

(deep 6)としてやると((((((pizza))))))ができますが
((((((pizza))))))ではなく((((((mozzarella))))))を作るにはどうしたら
いいでしょうか

話の流れ上、引数にするだけじゃない？とは言ってはいけないようです
こうです

```
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
```

(deep 4)にあたるものはこれ

```
(define four-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons p (quote ()))
       (quote ()))
      (quote ()))
     (quote ()))))
```

> もっと簡単な方法はないですか。

そうですねえ

> あります。

でも引数に持たせるだけではありません

> 13章の(letcc ...)を覚えていますか。
> はい。
> それが役に立ちます。
> まだ(letcc ...)の全貌を見ていなかったという意味でしょうか。
> 半分も見ていません。

こうです

```
(define toppings #f)
(define deepB
  (lambda (m)
    (cond ((zero? m)
           (let/cc jump
             (set! toppings jump)
             (quote pizza)))
          (else (cons (deepB (sub1 m)) (quote ()))))))
```

let/ccを呼んでいますが、jumpは覚えておくだけで使われていません
実行してみましょう
toppings(つまりjump)を関数のように使います
(toppings A)を評価すると、あたかもlet/ccがAを返したかのような振りをして、
さらにlet/ccが評価された後の作業を繰り返します

```
> (deepB 6)
'((((((pizza))))))

> (toppings (quote mozzarella))
'((((((mozzarella))))))

> (toppings (quote cake))
'((((((cake))))))

> (toppings (quote pizza))
'((((((pizza))))))
```

こういうわけです

では、

> ケーキにもう一段を加えてみましょう。

```
> (cons (toppings (quote cake)) (quote ()))
'((((((cake))))))
```

> きちんと動作しません。

そうですね

> 「(toppings m) を使うたびに、それは周りのすべてを忘れて、
> ちょうど6段のカッコを加えます。

後でやろうとしていたこと（ここではcons）のことを完全に忘れて
let/ccが評価された後の作業（6段のカッコを加える）を始めてしまい、
その勢いでREPLにまで戻ってきてしまうということですね

うまく使えるんでしょうかこの機能・・・
使えるんでしょうけど

> 第20の戒律
> (letcc ...) を用いて作られた値を考えるに際しては、同等ではあるが
> 忘れない関数を書き記すべし。そのあと、それを使用する際には、
> 忘れることを覚えておくべし。

忘れない関数って何でしょうか

ここで懐かしの&coの登場です

```
(define deep&co
  (lambda (m k)
    (cond ((zero? m) (k (quote pizza)))
          (else (deep&co (sub1 m)
                         (lambda (x) (k (cons x (quote ())))))))))
```

動きます

```
> (deep&co 0 (lambda (x) x))
'pizza

> (deep&co 6 (lambda (x) x))
'((((((pizza))))))

> (deep&co 2 (lambda (x) x))
'((pizza))
```

(deep&co 2 (lambda (x) x)) を追いかけてみます

```
  (deep&co 2 (lambda (x) x))
= (deep&co 1 (lambda (x) (k (cons x (quote ())))))
　  ※ kは(lambda (x) x)
= (deep&co 0 (lambda (x) (k (cons x (quote ())))))
　  ※ kは(lambda (x) (k2 (cons x (quote ()))))
　  ※ k2は(lambda (x) x)
= (deep&co 0 K) ※とおく
= (K (quote pizza))
```

式の置き換えがうまくいっているかどうかはこんな感じで確かめられます

```
> (let ((k2 (lambda (x) x)))
    (let ((k (lambda (x) (k2 (cons x (quote ()))))))
      (deep&co 0 (lambda (x) (k (cons x (quote ())))))))
'((pizza))
```

さらに続けると

```
k = (lambda (x) (k2 (cons x (quote ()))))
  = (lambda (x) ((lambda (x) x) (cons x (quote ()))))
  = (lambda (x) (cons x (quote ())))

K = (lambda (x) (k (cons x (quote ()))))
  = (lambda (x) ((lambda (x) (cons x (quote ()))) (cons x (quote ()))))
  = (lambda (x) (cons (cons x (quote ())) (quote ())))

(K (quote pizza)) = '((pizza))

```

となります
よく見るとKは次のtwo-layersと同じであることがわかります

```
(define two-layers
  (lambda (p)
    (cons
      (cons p (quote ()))
    (quote ()))))
```

deepからdeepBを作ったようにして、deep&coからdeep&coBを作ります

```
(define deep&coB
  (lambda (m k)
    (cond ((zero? m)
           (let ()
             (set! toppings k)
             (k (quote pizza))))
          (else
           (deep&coB (sub1 m)
                     (lambda (x) (k (cons x (quote ())))))))))
```

deepBと同じように使えます

```
> (deep&coB 2 (lambda (x) x))
'((pizza))

> (toppings (quote pizza))
'((pizza))
```

mが0のとき`(set! toppings k)`すること以外はdeep&coと同じです
`(set! toppings k)`するとtoppingsはさきほどのK、つまりtwo-layersになります
(deep&coB 6 (lambda (x) x))を評価すればsettingsはsix-layersに、
(deep&coB 4 (lambda (x) x))を評価すればsettingsはfour-layersになります

> それはつまり、最後の収集子は、deepBの中で(letcc ...)によって作られたものと
> 同等な関数に関係しているということですか。
> 
> はい。最後の収集子は、(letcc ...)が作った値の影法師です。

手習いで収集子が出てきたとき、「継続」と呼ばれることもありますと書いてありましたが
実際どこまで同じなのか、ちょっともやっとした気分でした
基本的に同じものであることがわかってすっきりです

ただし今度はtoppingsの上にトッピングを積み重ねることができます

```
> (cons (toppings (quote cake)) (quote ()))
'(((cake)))
```

たぶん、これが「忘れない関数」なのでしょう
なぜこれを書き記すべきなのかは考えてもわかりませんでした