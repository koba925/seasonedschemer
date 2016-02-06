# Scheme修行 (22) ジェネレータじゃないもの
> さてこれって継続使わなかったらどう書けるんですかね
> これは宿題！

骨組みはこんな感じでしょう

```
(define two-in-a-row*?
  (lambda (l)
    (letrec ((T (lambda (l)
                  (cond ((null? l) #f)
                        ((atom? (car l))
                         ...
                         (T (cdr l)))
                        (else
                         (or (T (car l)) (T (cdr l)))))))
             (T l)))))
```

あとは、直前の値さえ覚えておいて比較すればいいはず

```
(define two-in-a-row*?
  (lambda (l)
    (let ((prv (quote ())))
      (letrec ((T (lambda (l)
                    (cond ((null? l) #f)
                          ((atom? (car l))
                           (cond ((eq? (car l) prv) #t)
                                 (else
                                  (set! prv (car l))
                                  (T (cdr l)))))
                          (else
                           (or (T (car l)) (T (cdr l))))))))
        (T l)))))
```

これで動きました
継続版とどっちがいいでしょうねえ

難しい？と思ったのは
set!も使わないで書けるんじゃないかという話
イミュータブルとか関数型プログラミングとか言われてるご時世ですから

直前の値を引数に入れて渡してやれば、と思ったんですがcarで探してきた直前の値を
どうやってcdrに渡してあげればいいのやら

関数がふたつの値を返せればいいのかな
第10の戒律は「同時に2つ以上の値を集めるには関数を作るべし」でした
これってけっきょく収集子を作るって話だったと思うんですけど
まあリストを返せばいいか

```
(define pair
  (lambda (a b)
    (cons a (cons b (quote ())))))
(define val car)
(define prv cadr)

(define two-in-a-row*?
  (lambda (l)
    (letrec ((T (lambda (p l)
                  (cond ((null? l) (pair #f (quote())))
                        ((atom? (car l))
                         (cond ((eq? (car l) p) (pair #t (quote ())))
                               (else (T (car l) (cdr l)))))
                        (else (let ((vp (T p (car l))))
                                (cond ((val vp) (pair #t (quote ())))
                                      (else (T (prv vp) (cdr l))))))))))
      (val (T (quote ()) l)))))
```

悪くはないんですけどなんかごちゃごちゃしてる感じがしますね
ふたつの値を返すあたり構文しだいかもしれませんが

リストを渡り歩くという樹状の構造と、アトムを先頭から順番に探してくるという線形の構造が
いっしょくたになっているのがいけないように思います
つまり、やっぱりget-firstとget-nextがほしいってことかなあ
イミュータブルな世界だったらどうやって作るんだろう

といったところで、以前「そこまですんのかHaskell」と思ったことがあるのを思い出しました
たしか、リストの中を渡り歩くために今いるところの左側と右側のリストを
常に覚えているみたいな感じ
この関数では戻る必要が無いので右側だけ覚えておけばよさそうです
つまり、先頭のアトムと、先頭のアトムよりも右側のリストを返して
次はその返されたリストを渡してやれば次のアトムが取れると
なんていうか力技ですね

とにかく書いてみましたがあんまりわかりやすくはないです
Haskellのライブラリではもっとスマートに書いてあるんじゃないでしょうか

```
(define 1st car)
(define rest cadr)

(define 1st-and-rest
  (lambda (l)
    (cond ((null? l) (pair (quote ()) (quote())))
          ((atom? (car l)) (pair (car l) (cdr l)))
          (else (let ((a1r (1st-and-rest (car l))))
                  (let ((a1 (1st a1r))
                        (ar (rest a1r)))
                    (cond ((null? a1) (1st-and-rest (cdr l)))
                          (else (pair a1 (cons ar (cdr l)))))))))))
```

とりあえず動いている模様ではあります
カバー率？知りません

ということはtwo-in-a-row*?はすぐ書けますね
値の受け取り方にちょっと注意して

```
(define two-in-a-row*?
  (lambda (l)
    (letrec ((T (lambda (p l)
                  (let ((1r (1st-and-rest l)))
                    (cond ((null? (1st 1r)) #f)
                          ((eq? (1st 1r) p) #t)
                          (else (T (1st 1r) (rest 1r))))))))
      (T (quote ()) l))))
```

構造を分けることはできましたけどよくなったのかというと微妙な気分
もし仕事で書くとしたらどれで書くかなあ
普通にset!で前回の値を覚えてそうな気がする

ともあれ宿題は終了
