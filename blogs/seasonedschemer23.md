# Scheme修行 (23) テーブルとdefineとbox
ついにScheme修行も最終の第20章「店には何がある？」 
あいかわらずよく意味の分からないタイトルで締めてくれます

第10章とおなじく、ここではschemeのインタプリタを作ります
今回はdefineが（letccも）実装されますのでそのままセルフで自分自身を実行することができるはず

今回もなぜかテーブルの定義から入ります 好きですね
今回はリストではなく関数（というかクロージャ）でテーブルを作るそうです
関数でテーブルを作る利点は何でしょう？

まずは空っぽのテーブルを作ります

```
(define the-empty-table
  (lambda (name)
    (car (quote ()))))
```

↓のように書いてありますのでとりあえず版のようです

> 「Scheme手習い」では次のようにしました。
> `(car (quote ()))`

手習いの時はなんだろうこれと思ってたものですが
今回すっきり納得できるでしょうか
期待です

検索と追加です

```
(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond ((eq? name2 name1) value)
            (else (table name2))))))
```

なるほどなんか動きそうですね
使い方はこんな感じになるでしょうか

```
> (define test-table the-empty-table)
> (set! test-table (extend 'name 'taro test-table))
> (set! test-table (extend 'nationality 'japan test-table))
> (lookup test-table 'name)
'taro
> (lookup test-table 'nationality)
'japan
> (lookup test-table 'gender)
car: contract violation
  expected: pair?
  given: '()
```

このとき、test-tableはこんな風に育っています
変数名がかぶるのでちょっとわかりづらいですが

```
(lambda (name)
  (car (quote ())))
    ↓
(lambda (name2)
  (cond ((eq? name2 'name) 'taro)
        (else ((lambda (name)
                 (car (quote ()))) name2))))
    ↓
(lambda (name2)
  (cond ((eq? name2 'nationality) 'japan)
        (else ((lambda (name2)
                 (cond ((eq? name2 'name) 'taro)
                       (else ((lambda (name)
                                (car (quote ()))) name2)))) name2))))
```

確かに名前を与えると値を返す関数になっています

ところで(lookup table e)が(table e)と同じ意味なんであればlookupて必要なんですかね
何かと形がそろってる必要があるのかな？
必要はないけどそろえたい？
あとで何か気付きがあるでしょうか

テーブル作ったと思ったら今度は最上位のvalueを定義します ただし仮
ボトムアップだったりトップダウンだったり

```
(define value
  (lambda (e)
    (cond ((define? e) (*define e))
          (else (the-meaning e)))))
```

早速目玉機能であるdefineが現れました
完全に特別扱いです
the-meaningとやらの中では扱えないのかな？

defineです
どんな高尚なことをするのかと思ったら

```
(define *define
  (lambda (e)
    (set! global-table
          (extend (name-of e)
                  (box (the-meaning (right-side-of e)))
                  global-table))))
```

覚えておくだけかよ！
なぜ特別扱いなんだろう
the-meaningを読んでるけれどもthe-meaningの中で扱えないことはない気がするんだけど

覚えておく先はglobal-table決め打ちなんですね
ブロック構造なんかはこれでも実現できるんでしょうか
それとも実装しない？

それよりboxってなんでしょうか
入れものっぽい雰囲気は醸しだしてますが

> *defineが名前と値でテーブルを拡張すると、その名前はいつも「同じ」値を
> 表すようになりますか。
> 
> いいえ。前に何回か見たように、名前が表すものは(set! ...)を使って変更できます。
> 
> *defineがテーブルを拡張する前に値をboxに入れるのは、それが理由ですか。

それが理由らしいです
set!するためのしくみのようです

boxを作り、値を設定し、値を取り出す関数です。

```
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
```

短いですけど複雑ですね
まずは使ってみましょうか

```
> (define testbox (box 'a))
> (unbox testbox)
'a
> (setbox testbox 'b)
> (unbox testbox)
'b
```

いつものように追いかけてみましょう
defineあたりはてきとうにごまかしつつ

```
(define textbox (box 'a))
(define testbox (lambda (sel) (sel 'a (lambda (new) (set!
```

あれ？
(set! it new)のitってどうなるの？
今までそこはただ名前が書いてあるものだと思ってたけど、変数だったらどうなるの？
(set! 'a new)じゃ変だし・・・

そこはあくまでもitっていう名前で、変数じゃないってことかな？

```
> (let ((name 'aaa)) (define name 'bbb) name)
'bbb
```

そんな感じなのでとりあえずそういうことにしておいて進みます

```
(define textbox (box 'a))
(define testbox (lambda (sel) (sel 'a (lambda (new) (set! it new)))))

(unbox testbox)
(unbox (lambda (sel) (sel 'a (lambda (new) (set! it new)))))
((lambda (sel) (sel 'a (lambda (new) (set! it new)))) (lambda (it set) it))
((lambda (it set) it) 'a (lambda (new) (set! it new)))
'a
```

ふむふむよい感じ
続いてsetbox

```
(setbox testbox 'b)
(setbox (lambda (sel) (sel 'a (lambda (new) (set! it new)))) 'b)
((lambda (sel) (sel 'a (lambda (new) (set! it new)))) (lambda (it set) (set 'b)))
((lambda (it set) (set 'b)) 'a (lambda (new) (set! it new)))
((lambda (new) (set! it new)) 'b)
(set! it 'b)

(unbox testbox)
(unbox (lambda (sel) (sel 'a 
```

・・・
aがaのままじゃ何にもなってないじゃないか
ということはどうなんだ
結局itはitのままということか
仮引数のitと、boxに入ったitを区別するためにbox内のitは\<it>と書くことにして
最初からやってみよう

```
(define textbox (box 'a))
(define <it> 'a)
(define testbox (lambda (sel) (sel <it> (lambda (new) (set! <it> new)))))

(unbox testbox)
((lambda (sel) (sel <it> (lambda (new) (set! <it> new)))) (lambda (it set) it))
((lambda (it set) it) <it> (lambda (new) (set! <it> new)))
<it>
'a

(setbox testbox 'b)
((lambda (sel) (sel <it> (lambda (new) (set! <it> new)))) (lambda (it set) (set 'b)))
((lambda (it set) (set 'b)) <it> (lambda (new) (set! <it> new)))
((lambda (new) (set! <it> new)) 'b)
(set! <it> 'b)

(unbox testbox)
((lambda (sel) (sel <it> (lambda (new) (set! <it> new)))) (lambda (it set) it))
((lambda (it set) it) <it> (lambda (new) (set! <it> new)))
<it>
'b
```

defineの名前と、仮引数の名前、なんか違うものというイメージがありましたけど
おんなじってことですね
ぼんやりとは見えてきました

ところでわざわざboxなんてものを使わなくても、直接set!してはいけないんでしょうか

set!はdefineで定義した名前を指定するので、たとえば(a b)のbをcに
変更するってことができないから、ということかな
とうことは、名前の付いてない値を変更したい、でも名前がついてないとset!できない、
ということで隠れてこっそり名前をつけるのがboxということでしょうか
boxは、できないからしかたなく使うもので、本質的ではないと思ってもよいのでしょうか

ここで「名前」とか「値」とか言っているものが本当は何者なのか
あとでもう一度振り返ってみる必要がありそうです
コードができあがればはっきりするはず
