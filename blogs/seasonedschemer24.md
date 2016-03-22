# Scheme修行 (24) quote、identifier、set

the-meaningです

```
(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))
```

global-tableにおけるmeaningは特別ということでtheがついてるんでしょうね

> lookup-in-global-tableはテーブルと同じようなものというのは正しいですか。
> はい。名前を引数として取り、その名前と組になっている値をglobal-tableの中で
> 探します。

何を言いたいのかわかるようなわからないような感じですが
言ってることはそのとおりです

> つまり、lookup-in-global-tableはglobal-tableのようなものということですか。
> はいでもあり、いいえでもあります。
> *defineはglobal-tableを変更するので、lookup-in-global-tableは常に
> 最新のglobal-tableと同じようなものですが、現在のglobal-tableと同じ
> ようなものではありません。

てどういうことですかね
lookup-in-global-tableが現在のglobal-tableを探してくれないなんてことあるんでしょうか

・・・

直接global-tableって書いちゃうと、書いた時点のglobal-tableが
使われてしまって、あとでset!とかした内容が反映されないってことか
lookup-in-global-tableにすることで、実際に中身が必要になるまでテーブルの評価を
遅延させてる働きがあるんですね
Yでやったアレと同じ

> これを前に見たことがありますか？
> 16章のY!を思い出していますか？

いったんletで変数に割り当てた関数を変更してから呼ぶとき、
変更前の関数が呼ばれるのか変更後の関数が呼ばれるのかどっちなんだと思ってましたね
話の流れ上、変更後の関数が呼ばれるに違いないということで進みましたが
やはりそういうことだったぽいです

次はtheのないmeaning
こんどはtableを引数に取ります

```
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
```

「(expression-to-action e) e」てあたり、なんか回りくどくないですか
(action e table)みたいに作れないのかな
どっちみちeはactionに渡されるんだし

それだとactionが巨大なcondになってしまうからそれを避けてるんでしょうね
ポリモーフィズムみたいなものですね
ていうかこっちのほうがポリモーフィズムの元かな

ひとつ「action」を作ってみます

```
(define *quote
  (lambda (e table)
    (text-of e)))
```

なんとなく通して実行できそうな部品が揃ってきました
ちょっとずつ動かしながら進みたいので、フライングして後ろの方に出てくる
text-ofとかexpression-to-actionあたりの関数を入れておきます
全部入れるとまたあれもこれも入れないと動かないので余計なところはコメントアウトしたり

```
> (value '(quote a))
'a
```

quoteだけ評価できるインタプリタの完成です

```
(value '(quote a))
(the-meaning '(quote a))
(meaning '(quote a) lookup-in-global-table)
((expression-to-action '(quote a)) '(quote a) lookup-in-global-table)
(*quote '(quote a) lookup-in-global-table)
(text-of '(quote a))
'a
```

というわけです
actionが変わってもこのへんの流れは同じようなものだと思われます

次は*identifer
テーブルから探してくるだけですが値はboxに入っているのでunboxする必要があります

```
(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))
```

ええとこれは試す方法あるかな
テーブルに何か入っている必要がありますけど

ああ、今回はdefineがありますね
手習いの時は結局lambdaまで実装しないとテーブルに何も入らなくて
けっこう試しづらかった記憶が

```
> (value '(define a (quote aaa)))
> (value 'a)
'aaa
```

いけてるようです

続いてset!
目玉機能ですけどカンタン
ほとんどidentifierと同じ

```
(define *set
  (lambda (e table)
    (setbox (lookup table (name-of e))
            (meaning (right-side-of e) table))))
```

動きます

```
(上の続き)
> (value '(set! a (quote bbb)))
> (value 'a)
'bbb
```

動きはひとまとめにして追ってみます
defineとかset!とかが出てくると式だけ並べてもわかりません
boxとかテーブルのように式に式にかけないところの表現がなやましい
まあやってみます

```
global-table -> 空

(value '(define a (quote aaa)))
(*define '(define a (quote aaa)))
(set! global-table
  (extend (name-of '(define a (quote aaa)))
          (box (the-meaning (right-side-of '(define a (quote aaa)))))
          global-table))
(set! global-table (extend 'a 'aaa global-table))

global-table -> | a | 'aaa |

(value '(set! a (quote bbb)))
(the-meaning '(set! a (quote bbb)))
(meaning  '(set! a (quote bbb)) lookup-in-global-table)
((expression-to-action '(set! a (quote bbb)))
 '(set! a (quote bbb)) lookup-in-global-table))
(*set '(set! a (quote bbb)) lookup-in-global-table)
(setbox (lookup lookup-in-global-table 'a)
        (meaning '(quote bbb) lookup-in-global-table))
(setbox <aの指すbox> 'bbb)

global-table -> | a | 'bbb |

(value 'a)
(the-meaning 'a)
(meaning 'a lookup-in-global-table)
((expression-to-action 'a) 'a lookup-in-global-table)
(*identifier 'a lookup-in-global-table)
(unbox (lookup lookup-in-global-table 'a))
(unbox <aの指すbox>)
'bbb
```

てな感じです

set!はそのときのテーブルで、defineは常にglobal-tableで右辺を評価するところが違いますね
このへんがdefineを特別扱いする理由ぽい気がしてきました
the-meaning以下で呼ばれるとそのときのテーブルで評価されてしまいますので
（global-tableはグローバルなので使いたければいつでも使えますが自然に書けば）

defineが渡されたテーブル内で右辺を記録するようになっていたらどうなるでしょう
defineのあるスコープの範囲内でしか関数が呼び出せなくなってしまいますね
それはそれで使いみちがありそうな感じではありますが
手習いのvalueにdefineモドキをつけようとしたときそのへんで苦労したような気もします


