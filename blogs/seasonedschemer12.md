# Scheme修行(12) メモ化

## 「16. 位置について、セット、バン！」

こんな関数で引数をため込んでいきます。

```
(define ingredients (quote ()))
(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food
          (cons (quote cake)
                (quote ())))))
```

> 第16の戒律について忘れていませんか。

ずっと気になってました。
`set!`を`let`でくるむだけでは外から見えなくなってしまうし。

> 時には、戒律を無視したほうが説明が簡単になるからです。
> 今度`(set! ...)`を使うときには、`(let ...)`でつけた名前を使いましょう。

お手並み拝見と行きましょう。

## メモ化

次は`pizza`をカッコでトッピングする`deep` です。

```
(define deep
  (lambda (m)
    (cond ((zero? m) (quote pizza))
          (else (cons (deep (sub1 m)) (quote ()))))))
```

引数と結果をため込んでいくようにします。

```
(define Ns (quote ()))
(define Rs (quote ()))
(define deepR
  (lambda (n)
    (let ((result (deep n)))
    (set! Rs (cons result Rs))
    (set! Ns (cons n Ns))
    result)))
```

なんかさっきの約束が無視されてる気がしますが。

せっかくため込んだので、引数が同じだったらため込んだ中から値を返します。メモ化ってやつでしょうか。

```
(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond ((= (car ns) n) (car rs))
                    (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deepM2
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))
```

いったん`member`で`n`が`Ns`に含まれるかどうかを確認してから`find`しているのがもったいない感じですが、メモ化のパターンみたいなものですかね。
おかげで`find`が`null?`を確認する必要はなくなってますが。

## メモ化の最適化？なの？

> `deep`の中での再帰を`(deep (sub1 m))`から`(deepM (sub1 m))`に変更して、`deep`を手助けすべきでしょうか。

なるほど。

```
(define deep3
  (lambda (m)
    (cond ((zero? m) (quote pizza))
          (else (cons (deepM3 (sub1 m)) (quote ()))))))

(define deepM3
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep3 n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))
```

しかしこれ、いきなり`(deepM 10000000)`とかやっちゃったら大変なことになりますね。
メモ化というよりはテーブル作ってるのに近い感じ。
呼び出され方次第でしょうか。

## 約束

> `deepM`はまだ第16の戒律に反しています。

そうですね。
```
(define deepM4
  (let ((Ns (quote ()))
        (Rs (quote ())))
    (lambda (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((result (deep4 n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))
```

第16の戒律が出てから初めて、`let`で定義した値がまともに役に立ちました。

実行結果はわかりますが、実行中にちゃんと想定通り動いてることを確認するにはデバッガで追いかけるしかないですかね。ですよね。