# Scheme修行(7) さらにletcc

## rember-beyond-first

つぎは`rember`をネタにします。
`rember`を`letrec`を使って書き換えます。
次に、最初に見つけたアトムだけでなく、それ以降のアトムをすべて削除する、`rember-beyond-first`を作ります。
これは`rember`を1行書き換えるだけで、letccは使わずに書くことができます。
わざわざ取り上げることもなかったような気がしますが何か裏があるのでしょうか。
5回読めばわかると思うので今は気にせずに進みます。

## my-rember-upto-last

こんどは最後に見つかるアトムとそれ以降のアトムのみを残す`rember-upto-last`を作ります。
まずは自分で作ってみます。
これはぱっと考えても`rember`や`rember-beyond-first`よりも複雑なやり方が必要そうです。
普通に作るなら後ろから見ていくんでしょうかね。
配列ならいいんですけどリストは後ろからたどるのってちょっと苦手なんですよね。
ひっくり返して前から見ますか。

```
(define reverse
  (letrec
      ((R (lambda (lat rev)
            (cond ((null? lat) rev)
                  (else (R (cdr lat) (cons (car lat) rev)))))))
    (lambda (lat) (R lat (quote ())))))

(define my-rember-upto-last
  (lambda (a lat)
    (letrec
        ((R (lambda (rlat result)
              (cond ((null? rlat) result)
                    ((eq? (car rlat) a) result)
                    (else (R (cdr rlat) (cons (car rlat) result)))))))
    (R (reverse lat) (quote ())))))
```

そういえばこのシリーズではこうやって結果を変数に蓄積していくやり方って出てきませんね。
なにか邪道っぽいことなんでしょうか。
この技なしでひっくり返そうとするとけっこうめんどくさそうなんですが。

## remember-upto-last

こんどは`letcc`を使います。
使うんだろうなあと思いつつも自力ではどう使うのかわかりませんでした。

> 新しい版では、アトム`a`を見つけると、`lat`の要素をみるのは止めませんが、そこまでに見たすべてのものを捨ててしまいます。

忘れるために`letcc`を使うんだろうということはわかるんですが。

> 探し続けるけれども、結果に`cons`しようと舞っているアトムを無視するにはどうすればよいですか。

そうそう、アトムを見つけても「あとで`cons`する」が残ってると`multirember`になってしまうんですよねえ。
というわけで答えです！

```
(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
      (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) (quote ()))
                  ((eq? (car lat) a) (skip (R (cdr lat))))
                  (else (cons (car lat) (R (cdr lat))))))))
        (R lat)))))
```

なるほど！そうか脱出するだけじゃないんだ。追ってみましょう。

```
(rember-upto-last 'a '(b a c a d))
(let/cc skip (R '(b a c a d)))
(let/cc skip (cons 'b (R '(a c a d))))
(let/cc skip (cons 'b (skip (R '(c a d)))))
(let/cc skip (cons 'b (skip (cons 'c (R '(a d))))))
(let/cc skip (cons 'b (skip (cons 'c (skip (R '(d)))))))
(let/cc skip (cons 'b (skip (cons 'c (skip (cons 'd (R '())))))))
(let/cc skip (cons 'b (skip (cons 'c (skip (cons 'd '()))))))
(let/cc skip (cons 'b (skip (cons 'c (skip '(d))))))
(let/cc skip '(d))
'(d)
```

見事に忘れていますね。

## 手習いで出てきた継続との関係

そういえばこの本では訳者まえがき以降「継続」っていう言葉は出てきてない気がしますが`(letcc hop`の`hop`が「継続」のはず。
手習いで出てきた「継続」（収集子とも言ってましたが）との関係が気になります。
同じもののことを言ってるんでしょうか？

手習い風の継続を使って`rember-upto-last`を書いてみます。

```
(define rember-upto-last&co
  (lambda (a lat)
    (letrec
        ((R (lambda (lat col)
              (cond
                ((null? lat) (col (quote ())))
                ((eq? (car lat) a)
                 (R (cdr lat) (lambda (x) x)))
                (else
                 (R (cdr lat) (lambda (x) (col (cons (car lat) x)))))))))
      (R lat (lambda (x) x)))))
```

ぴったり同じかというと微妙ですが、全体としては同じ構造で書けました。
お前の仕事が終わったらこれやっとけよ、というのを`col`に貯めていくわけですが
`a`が見つかった時、それまでに貯めた`col`を一度忘れて、`(lambda (x) x)`にリセットしてしまうというのが`(skip ...)`に対応しています。

今はそれだけですが、きっと先まで読めばわかるんじゃないでしょうか。
13章はここでおしまい。