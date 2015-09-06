# Scheme修行(9) letとletcc

## leftmostをいじる

`let`を使った`leftmost`です。

```
(define leftmost3
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost3 (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost3 (cdr l)))))))))
```

`let`を使うことにより同じ式を2回評価するという無駄を減らしましたが、まだ無駄があります。
簡単なケースとして`(leftmost '(((s))))`を評価する場合、再帰を繰り返して`'s`にたどり着いた後、それに`a`という名前をつけ、`a`つまり`'s`はアトムなので`'s`を返す、ということを繰り返して値が求まります。
1ステップずつ追うとこうです。関数名は`lm`と略記しました。

```
(lm '(((s))))
(let ((a (lm '((s))))) (cond ...))
(let ((a (let ((a (lm '(s)))) (cond ...)))) (cond ...))
(let ((a (let ((a 's)) (cond ...)))) (cond ...))
(let ((a 's)) (cond ...))
's
```

答えは`'s`だとわかっているのにひとつずつ戻っていくのは無駄ですね。
そこで、答えが求まったらすぐに値を返すようにします。
`letcc`を使います。

```
(define leftmost5
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) (quote ()))
                   ((atom? (car l)) (skip (car l)))
                   (else
                    (let ((a (lm (car l))))
                      (cond
                        ((atom? a) a)
                        (else (lm (cdr l))))))))))
        (lm l)))))
```

ステップはこんな感じです。

```
(leftmost5 '(((s))))
(let/cc skip (lm '(((s)))))
(let/cc skip (let ((a (lm '((s))))) (cond ...)))
(let/cc skip (let ((a (let ((a (lm '(s)))) (cond ...)))) (cond ...)))
(let/cc skip (let ((a (let ((a (skip 's))) (cond ...)))) (cond ...)))
's
```

終わりが速いですね。

・・・と思ったら、正解は違ってました！
そもそも`a`と名前を付ける必要もありません。

```
(define leftmost7
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond ((null? l) (quote ()))
                       ((atom? (car l)) (skip (car l)))
                       (else (let ()
                               (lm (car l))
                               (lm (cdr l))))))))
        (lm l)))))
```

動き方は同じ。

```
(leftmost7 '(((s))))
(let/cc skip (lm '(((s)))))
(let/cc skip (let () (lm '((s))) (lm '())))
(let/cc skip (let () (let () (lm '(s)) (lm '())) (lm '())))
(let/cc skip (let () (let () (skip 's) (lm '())) (lm '())))
's
```

テキストの注には`let ()`の代わりに`begin`とも書けるよ、と書いてありますが実はそれも不要で、これで動きます。

```
                       (else 
                        (lm (car l))
                        (lm (cdr l)))))))
```

`cond`の後ろにはひとつしか式が書けないということにしておきたいんでしょうか。
こだわりポイントが何なのかちょっとわかりません。
動き方を同じように書こうとするとこんな風になると思いますが、正しさに少々不安が。

```
(leftmost8 '(((s))))
(let/cc skip (lm '(((s)))) (lm '()))
(let/cc skip (lm '((s))) (lm '()) (lm '()))
(let/cc skip (lm '(s)) (lm '()) (lm '()) (lm '()))
(let/cc skip (skip 's) (lm '()) (lm '()) (lm '())))
's
```

そういう不安を起こさないように、ということかもしれません。

## letccは関数型

純粋に関数型のプログラムなら、複数の式を順番に実行する機能は必要ない。
そんな風に考えていた時期が俺にもありました。
ていうかそうですよね？
副作用がないということは関数の値しか使えるものがないのにそれを捨ててしまうということですから。

ところがletccを使うと順番に実行する機能の使いでが発生する。
letccを使ったプログラムは（いわゆる）関数型の範疇を飛び出してるってことになるんでしょうか。
それまでにやろうとしてたことを忘れてしまうというのは副作用といえばこの上ない副作用ですしね。
letccで得た継続の値を持ち出さない限り影響が外に及ぶことはなさそうですが。