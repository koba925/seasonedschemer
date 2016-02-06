# Scheme修行 (20) ジェネレータ？（前編）
two-in-a-row?です

```
(define two-in-a-row?
  (letrec ((W (lambda (a lat)
                (cond ((null? lat) #f)
                      (else (let ((nxt (car lat)))
                              (or (eq? nxt a)
                                  (W nxt (cdr lat)))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
            (else (W (car lat) (cdr lat)))))))
```

two-in-a-row*?を作ります
latじゃなくて一般のリストを相手にするやつです

まずはこういうのを考えてみます

```
(define leave #f)
(define fill #f)

(define waddle
  (lambda (l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (let ()
             (let/cc rest
               (set! fill rest)
               (leave (car l)))
             (waddle (cdr l))))
          (else (let ()
                  (waddle (car l))
                  (waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l))))

(define get-next
  (lambda (x)
    (let/cc here-again
      (set! leave here-again)
      (fill (quote go)))))
```

なんでしょうかこれは
実行してみます

```
> (start-it2 '((donuts) (cheerios (cheerios (spaghettios))) donuts))
'donuts

> (get-next (quote go))
'cheerios

> (get-next (quote go))
'cheerios

> (get-next (quote go))
'spaghettios

> (get-next (quote go))
'donuts

> (get-next (quote go))
'()
```

どうやらリストの左から順にアトムを取り出してくれるようです
start-it2が最初のアトムを取り出し、get-nextで次々とアトムを取ってくる
実にいい感じです

どういうしくみなんでしょうか
start-it2はこうです

1. leaveに現在の継続をセットして
2. ((donuts) (cheerios (cheerios (spaghettios))) donuts)にwaddleを適用する

waddleがdonutsに達するとこうなります

1. restに現在の継続をセットして
2. donutsにleaveを適用する

するとleaveがstart-it2の続きを実行しますのでstart-it2がdonutsを返すというわけですね

このときのrestはどんなものでしょうか

> restに相当する関数を定義できますか。
> 
> 問題ありません。
> 
> `(define rest1 (lambda (x) (waddle l1)))`
> 
> ここで
> 
> l1は`(() (cheerios (cheerios (spaghettios))) donuts)`です。

ここが評価される時点でのlは(donuts)ですのでホントは違う気がしますが
全体の流れの中ではそうとも言えるでしょうか
ひとつめのアトムの処理が済んだところ、ということですね

次にget-nextを呼ぶと

1. leaveに現在の継続をセットして
2. goにfillを適用する

leaveは、後でここに戻ってくるために使われているっぽいですね

fillを呼ぶと、さきほど中断したところから実行が再開されます
渡された値(go)はlet/ccの値となりますが、何も使われずに捨てられています
上のrestが引数のxを使っていないことに対応します

waddleがcheeriosに達すると

1. restに現在の継続をセットして
2. donutsにleaveを適用する

するとleaveがget-nextの続きを実行しますのでget-nextがcheeriosを返してくれます
その調子でget-nextはcheerios、spaghettios、donutsを返してくれますが
最後にget-nextを呼ぶと

> わお！

何がわおなんでしょうか

> waddleはついに(null? l)にたどり着きます。

そうですね

> ともあれ、最後の値は()です。

そうでした
何が問題なのでしょうか
いいじゃないですか()で

> もし、しようとしていたことがすべて終わったのなら、最初に(start-it l)の
> 値を尋ねたところに戻るはずです。

・・・よくわかりません

leaveとかfillで流れが分断されてはいるものの
leaveとかfillでジャンプする前には必ず現在の場所を覚えてて
後でその直後に戻ってくるようにはなっているので
いつかは元の場所に戻るんだよといわれればそのとおり

でも1行ずつ追うように読んでいくとかえってわかりにくくなります
順番に呼んでいくところはイメージしやすいですが戻るところはイメージしづらい

具体例でキッチリ読んでいってみます
元の引数だと大変そうなので簡単な引数で

```
(start-it2 '(a b))
```

このあとどう書くとすっきりするのか考えてみましたがいいアイデアがありません
だらだら書いていきます

これはこうなります

```
(letcc here
  (set! leave here)
  (waddle '(a b)))
```

そうするとまず「leaveに現在の継続をセットする」わけですが
「現在の継続」ってなんでしょうか
どこに戻るかってことを考えるには具体的にしておかなくてはなりません
関数の定義に遡ると(@はマークとして挿入)

```
(define start-it2
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l)))@)
```

leaveの引数を(let/cc ...)の値として、@のところから実行を再開する
というのがleaveの内容と言えそうです

今は終わり方を調べようとしてますので
実行を再開した後はどこまで実行されて終わるのかも気になります
この場合、すぐREPLに値を返してしまうのでそこで終わるしかありませんが
これ以外でもREPLに戻るまで実行が続くということでいいんでしょうか
そういうことにしておきます

1. leaveの引数を(let/cc ...)の値として、@のところ(let/ccの直後)から実行を再開する
2. （何もすることがないのでそのまま）REPLに値を返す（終了）

という理解です

次は(waddle '(a b))を評価します
(waddle '(a b))を評価し終えれば、その値が(start-it2 '(a b))の値にもなるはずですが
どうでしょうか

(waddle '(a b))はこうなります

```
(let ()
  (let/cc rest
    (set! fill rest)
    (leave 'a))
  (waddle '(b))))
```

さっきと同じように考えるとfillにはこんな継続が設定されるはず

1. fillの引数を(let/cc ...)の値とするけれども誰も見ていないので捨てる
2. (waddle '(b))から実行を再開する
3. その値を(start-it2 '(a b))に返す
4. (start-it2 '(a b))はその値をREPLに返す（終了）

そしてleaveです
さっき覚えた継続を実行しますので

(let/cc ...)の値がaとなり、それがREPLに返されて終了です
合ってます

次です

```
(get-next (quote go))
```

はこうです

```
(let/cc here-again
  (set! leave here-again)
  (fill (quote go)))
```

leaveはこういう継続

1. leaveの引数を(let/cc ...)の値として、let/ccの直後から実行を再開する
2. （何もすることがないのでそのまま）REPLに値を返す（終了）

そしてfillを実行
fillの引数は捨てて(waddle '(b))から実行を再開します

(waddle '(b))はこうなります

```
(let ()
  (let/cc rest
    (set! fill rest)
    (leave 'b))
  (waddle '())))
```

fillにはこんな継続が設定されます

1. fillの引数を(let/cc ...)の値とするけれども誰も見ていないので捨てる
2. (waddle '())から実行を再開する
3. (waddle '(b))は(waddle '())の値を(start-it2 '(a b))に返す
4. (start-it2 '(a b))はその値をREPLに返す（終了）

そしてleaveです
さっき覚えた継続を実行しますので

(let/cc ...)の値が'aとなり、それがREPLに返されて終了です
合ってます

最後の(get-next (quote go))を実行します

```
(let/cc here-again
  (set! leave here-again)
  (fill (quote go)))
```

leaveは（略）

そしてfillを実行
fillの引数は捨てて(waddle '())から実行を再開します

(waddle '())はすぐに()を返します
すると2. 3. 4.を通ってstart-it2が()をREPLに()を返す、というわけですね
やっと納得できました

もうちょっとコンパクトに書けないものかなあ

で、()を返すなら何が問題なんでしょうか

ファイルに

```
(start-it2 '(a b))
(get-next (quote go))
(get-next (quote go))
```

と書いて実行しても

```
'a
'b
'()
```

という結果が表示されるだけでした

しかし、本当にstart-it2に戻っているのであれば、'()を返した後
無限ループになってもよさそうなものです

もしかして、ファイルに書いてあってもトップレベル（って言うんだっけ）まで
戻ったところで継続は止まってしまうのかな？
そうするとREPLと同じ
値が表示されるってことはそういうことかな
REPLに返すというよりトップレベルに返す、と書いたほうがよかったかもしれない

というわけで無理やりletでくるんでみました

```
> (let ()
    (start-it2 '(a b))
    (get-next (quote go))
    (get-next (quote go)))
 
```

固まってます！
もう少し様子がわかるようにしてみます

```
> (let ()
    (display "1 ") (display (start-it2 '(a b))) (newline)
    (display "2 ") (display (get-next (quote go))) (newline)
    (display "3 ") (display (get-next (quote go))) (newline))
1 a
2 b
3 ()
2 ()
2 ()
2 ()
 :
 :
```

letでくるんだことにより、fillの継続の最後が
「(start-it2 '(a b))はその値をREPLに返す（終了）」だったところが
「(start-it2 '(a b))の値を表示して次に(get-next ...)を評価する」に
なったというわけです

get-nextは同じfillを実行し続けるので最後の部分を繰り返し実行し続ける、と
ふー

やっと

> では、リストが完全に消費されたあとで(leave (quote ()))を使うと
> うまくいきそうだ、というのは正しいですか。

になるほど、とうなずくことができます

最後にstart-it2に戻ってきたあとleaveを呼ぶと、最後のget-nextでsetした
leaveが実行されるので、start-it2ではなくget-nextが()を返して終わってくれます

```
> (let ()
    (display "1 ") (display (get-first '(a b))) (newline)
    (display "2 ") (display (get-next (quote go))) (newline)
    (display "3 ") (display (get-next (quote go))) (newline))
1 a
2 b
3 ()

> 
```

結局、ジェネレータみたいなものを作ってみたということ？
leaveがyield、fillがnextにあたりそう
（使ったことないのでいいかげんなこと言ってますけどたぶん）

two-in-a-row*?の話はまたこんど