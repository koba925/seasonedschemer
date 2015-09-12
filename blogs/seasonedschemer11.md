# Scheme修行(11) 代入

## 15. 大人と子供の違い・・・・・・

`set!`は`define`された変数に値を代入します。
と書くと先生に怒られるかもしれません。

（たぶん）ここで初めて「名前xはaを参照しています」という言い方が出てきました。
こちらが正しい表現なのでしょう。
さっきまでaを参照していたxに、今度はbを参照させるというのが`set!`ということになります。

## `set!`と`define`

`set!`はすでに`define`された名前についてのみ使用可能です。
実際に、定義されてない名前を`set!`しようとするとエラーになります。
逆に、`define`済みの名前でもう一度`define`しようとするのもエラーかと思いきや成功。

```
> (define x
    (cons (quote chicago)
          (cons (quote pizza)
                (quote ()))))
> x
'(chicago pizza)
> (set! x (quote gone))
> x
'gone
> (set! y (quote yet))
 set!: assignment disallowed;
 cannot set variable before its definition
  variable: y
> (define x (quote again))
> x
'again
```

しかしこういうファイルを作って...

```
(define x
  (cons (quote chicago)
        (cons (quote pizza)
              (quote ()))))
x

(set! x (quote gone))
x

(define x (quote again))
x
```

実行するとエラーになりました。
REPL上では動作が違う？

```
module: duplicate definition for identifier in: x
```

REPL上で`define`しなおすことができないと試行錯誤できないからそれでいいのかな。
`define`で値を変更できるようにすれば`set!`はいらなくなりそうですが、あえて好き勝手できないようになってるんでしょうね。
未定義の変数に`set!`できないのも同様なんでしょう。

## 値

`(define ...)`と`(set! ...)`は値を持ちません。
なぜかしつこいほど値を聞かれますが値はありません。

## 変わる

こう定義して

```
(define x (quote skins))

(define gourmet
  (lambda (food)
    (cons food
          (cons x (quote ())))))
```

`(gourmet (quote onion))`を評価すると当然`(onion skins)`ですが`(set! x (quote rings))`した後もう一度`(gourmet (quote onion))`を評価するとなんと！`(onion rings)`になります。
いやそれも当然っぽいんですけど。

ただ、手習いで作ったクロージャだと、クロージャができた後は値の変わりようがない気がするので、当然ともいいきれないなあと。

## 状態

現時点での変数の値（正確に言うと、「名前が参照する値」？）を覚えておかないといけないので本が読みづらくなりました。
やっぱり状態を持つのはよくないですね！（メモリ不足

## 隠す

その後、最後に食べたものを覚えておけるこんな関数を作ります。

```
(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x (quote ())))))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons (quote milkshake)
          (cons food (quote ())))))
```

しかし、`x`がカブっているため、思った通りには動きません。
他の関数からの影響を受けないようにするため、こんな風にして`x`を隠します。

```
(define omnivore
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x (quote ()))))))
```

`(omnivore (quote (bouillabaisse))`を評価しても、関数の外側で定義した`x`は影響を受けません。
別の関数が`x`という名前を使っても（ちゃんと隠していれば）影響を受けることはありません。

```
(define gobbler
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x (quote ()))))))
```

staticなローカル変数といった感じです。

しかし残念ながら、`x`が隠されてしまっているので最後に食べたものを確認することはできなくなってしまいました。
関数内部で前回の値を使うような例になっていればまだ役に立っているように見えるのですが。
この辺りはきっと後で解決されるのでしょう。

`ominivore`の値は何ですか、という問が繰り返され、結局関数です、ということになります。
`lambda`なんだから関数なのはわかっているんですが、`let`の中に入ってても同じか、と聞いているのかなあ？
詳しく言うと、`lambda`で定義した関数と`let`で定義した`x`を含むクロージャ、となると思うんですがそう言わないということは？
手習いを読んでいればその説明で飲み込めないことはないと思うんですが。

## 第16の戒律

> `(let ...)`で定義された名前に対してのみ`(set! ...)`を使うべし。

`let`で定義するのはいいとして、`minestrone`みたいな捨てられるだけの値を書くのはシャクに触るので書かずにすませたいところですが・・・

## 間違い

これはうまくいきません。

```
(define nibbler
  (lambda (food)
    (let ((x (quote donut)))
      (set! x food)
      (cons food
            (cons x (quote ()))))))
```

`nibbler`を評価するたびに`x`が新しく定義されるので、前回の値を覚えておく役にたっていません。
といっても評価した値は変わらないので、`x`覚えてないよね、というのは脳内で確かめるしかありませんが。

これでうまくいけばわかりやすいんですが、これはこれでうまくいきません。

```
(define omnivore2
  (let ((x (quote minestrone)))
    (lambda (food)
      (cons food
            (cons x (quote ())))
      (set! x food))))

(define nibbler2
  (lambda (food)
    (let ((x (quote donut)))
      (cons food
            (cons x (quote ())))
      (set! x food))))
```

`set!`が値を返してくれないので・・・

しつこいほど繰り返してもらったのに`set!`は値を持たないことを忘れててしばらくなんでうまくいかないのか考えてしまいました。

## 第17の戒律(予備版)

> `(let ((x ...)) ...)`に 対して`(set! x)`を用いる際には、それらの間に少なくとも１つの`(lambda ...`を置くべし。

というよりも、`lambda`の外側に`let`を置け、ってことのような気がします。

## swap

代入を使って値を入れ替える関数を作ります。

```
(define chez-nous
  (lambda ()
    (set! food x)
    (set! x food)))
```

失敗です。わざとらしいですね。こうです。

```
(define chez-nous2
  (lambda ()
    (let ((a food))
    (set! food x)
    (set! x a))))
```

## 第18の戒律

> `(set! x ...)`は`x`が参照する値がもはや必要ないときにのみ使うべし。

そりゃそうですね。

## まとめ

大人と子供の違いって？

## git

しまったmasterで作業してた！
しかもpushしてから気づく始末

ダレてるな

でも仕事で使ってたって3回に1回くらいは忘れそう

ごまかしのスキルもつけないとなあ（そこ