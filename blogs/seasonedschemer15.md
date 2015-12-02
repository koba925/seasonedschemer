# Scheme修行 15 状態

「17. 我変わる、ゆえに我あり！」ではふたたびdeepMをいじります

## deepMの改良(？)

deepを内部に持つバージョンから始めます

```
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (letrec ((D (lambda (m)
                  (if (zero? m)
                      (quote pizza)
                      (cons (D (sub1 m)) (quote ()))))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))
```

これはメモ化が十分に働かない半バグバージョン
これを何度か修正して、以下のような形にします

```
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       (quote pizza)
                       (cons (deepM (sub1 n)) (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
```
この間にやったことは以下のとおり

1. 正しくメモ化が動くようにする
1. Dの呼び出しをdeepMに変えるだけ
1. letrecをletに変更する
1. 自分自身を呼んでいないので
1. ふたつのletをひとつにまとめる
1. 一度しか呼ばれていないDを、lambdaに置き換える
1. (lambda (m) (...) n) を (let ((m n)) ...)に書き換える
1. (let ((m n)) ...) からmを消す

ここでは何を教えてくれているのでしょうか？

- 式の変形の練習みたいなもの？
- なにか計算機科学的な背景がある？
- 次にやること(consの回数を数える)の準備なだけ？

狙いがわかりませんでした

## deepMのconsを数える

呼ばれた回数を数えるconsを作ります

```
(define counter #f)
(define set-counter #f)
(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
```

(set! counter (lambda () N)) というのがちょっと不思議な感じですが
こんなふうにNを直接見せてもやりたいことはやれるんですけれども

```
(define N 0)

(define consC
  (lambda (x y)
    (set! N (add1 N))
    (cons x y)))
```

好き放題されてしまうので行儀がよろしくないということでしょう
Javaで言うところのSetter、Getterみたいな感じですね
外から勝手に使うことのできない状態をクロージャに持つことができました

なお(define counter #f)の#fには意味はありません
(define counter)でもいいしそれで通る処理系もあるのですが
Racketでは通らなかったのでとりあえず#fと書いてます

consCを呼ぶようにdeepを書き換えて

```
(define deep
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (consC (deep (sub1 m)) (quote ())))))
```

(deep 0)から(deep 1000)まで実行してやると

```
(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(set-counter 0)
(supercounter deep)
```

500500回consを実行したことがわかります

> ありがとう Carl F. Gauss (1777-1855)。

deepMのconsを数えてやると

```
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       (quote pizza)
                       (consC (deepM (sub1 n)) (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(set-counter 0)
(supercounter deepM)
```

メモ化の効果で1000回で済んでいることがわかります
だから速くなったのかというとちょっとわかりませんが

## rember1*のconsを数える

14章のrember1*のconsを数えます
最初に作った版では、引数に取ったリストをそのまま返すだけでいい場合でも
１からリストを作っていたので余分なconsを行っていました
継続を使った版では、指定されたアトムがリスト内に存在しなければ
継続で即抜けて、指定されたリストをそのまま返しますので無駄なconsは実行されません
と言いたいだけなのか、もっと大事なことを言おうとしているのかわかりません
