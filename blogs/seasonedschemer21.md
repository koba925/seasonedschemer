# Scheme修行 (21) ジェネレータ？（後編）

前回のwaddleはもともと再帰がややこしいところにlet/ccが入ったので
なんだかよくわからないことになってましたが
leaveとfillだけの話ならこんな感じでぴょんぴょんさせることができます

```
(define A
  (lambda ()
    (let/cc here (set! leave here)
      (display "A1") (newline)
      (B))
    (let/cc here (set! leave here)
      (display "A2") (newline)
      (fill))
    (let/cc here (set! leave here)
      (display "A3") (newline)
      (fill))
    (display "A4") (newline)))

(define B
  (lambda ()
    (let/cc here (set! fill here)
      (display "B1") (newline)
      (leave))
    (let/cc here (set! fill here)
      (display "B2") (newline)
      (leave))
    (let/cc here (set! fill here)
      (display "B3") (newline)
      (leave))))
```

get-firstの最後のleaveに相当するものはどこへ行ってしまったのか少し心配ですが
思った通りには動いてくれます

```
> (A)
A1
B1
A2
B2
A3
B3
A4
```

こういうのをたぶんコルーチンとも言います

もっとよく理解するために、収集子を使った形に直してみます
忘れない関数を覚えおくべし、って言ってましたしね（意味がよくわかってないけど）

```
(define get-first&co
  (lambda (l)
    (let ((here (lambda (x) x)))
      (set! leave here)
      (waddle&co l here))))

(define get-next&co
  (lambda ()
    (let ((here-again (lambda (x) x)))
      (set! leave here-again)
      (fill (quote go)))))

(define waddle&co
  (lambda (l col)
    (cond ((null? l) (col (quote ())))
          ((atom? (car l))
           (let ((rest (lambda (x) (waddle&co (cdr l) col))))
             (set! fill rest)
             (leave (car l))))
          (else (waddle&co
                 (car l)
                 (lambda (x) (waddle&co (cdr l) col)))))))
```

なんとなくこんな感じかな、と思って書いてみたら動いてしまって
なぜちゃんと動くのかちっともわかりません
もしかしたらたまたまかも

さらに削って、途中で脱出しない版を作ってみます
これはScheme手習いの復習ですね

```
(define waddle&co
  (lambda (l col)
    (cond ((null? l) (col (quote ())))
          ((atom? (car l))
           (display (car l)) (newline)
           (waddle&co (cdr l) col))
          (else (waddle&co (car l) (lambda (x) (waddle&co (cdr l) col)))))))
```

何もしないと途中何が起こっているのかわからないのでdisplayを入れました
(((a)) b)に適用してみます

```
(waddle&co '(((a)) b) (lambda (x) x))
(waddle&co '((a)) (lambda (x) (waddle&co '(b) (lambda (x) x))))
(waddle&co '(a) (lambda (x) (waddle&co '()
                  (lambda (x) (waddle&co '(b) (lambda (x) x))))))
; (display 'a) (newline)
(waddle&co '() (lambda (x) (waddle&co '()
                 (lambda (x) (waddle&co '(b) (lambda (x) x))))))
((lambda (x) (waddle&co '()
   (lambda (x) (waddle&co '(b) (lambda (x) x))))) (quote ()))
(waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))
((lambda (x) (waddle&co '(b) (lambda (x) x))) (quote ()))
(waddle&co '(b) (lambda (x) x))
; (display 'b) (newline)
(waddle&co '() (lambda (x) x))
((lambda (x) x) (quote ()))
(quote ())
```

colの値は捨てられるだけなのであまり複雑にならず多少追いかけやすいです

もとの定義では、この流れがところどころでぶったぎられる形になっているはず

```
(get-first&co '(((a)) b))
; (set leave (lambda (x) x))
(waddle&co '(((a)) b) (lambda (x) x))
(waddle&co '((a)) (lambda (x) (waddle&co '(b) (lambda (x) x))))
(waddle&co '(a) (lambda (x) (waddle&co '()
                  (lambda (x) (waddle&co '(b) (lambda (x) x))))))
; (set fill (lambda (x) (waddle&co '()
;             (lambda (x) (waddle&co '(b) (lambda (x) x))))))
(leave 'a)
((lambda (x) x) 'a)
'a

(get-next&co)
; (set leave (lambda (x) x))
(fill (quote go))
((lambda (x) (waddle&co '()
   (lambda (x) (waddle&co '(b) (lambda (x) x))))) (quote go))
(waddle&co '() (lambda (x) (waddle&co '(b) (lambda (x) x))))
((lambda (x) (waddle&co '(b) (lambda (x) x))) (quote ()))
(waddle&co '(b) (lambda (x) x))
; (set fill (lambda (x) (waddle&co '() (lambda (x) x))))
(leave 'b)
((lambda (x) x) 'b)
'b

(get-next&co)
; (set leave (lambda (x) x))
(fill (quote go))
((lambda (x) (waddle&co '() (lambda (x) x))) (quote go))
(waddle&co '() (lambda (x) x))
((lambda (x) x) (quote ()))
(quote ())
'()
```

ふむ
そんな感じですね

脱出のためのしくみは何も使っていないのに、同じことができてるのが面白いです
収集子を使う形にしたとき、全体が末尾再帰の形になったから、かな
lambdaすげえ

ちゃんと動くし変なコードを書いてたりするわけでもなさそうです
でもまだ収集子が育って評価される様子がぱっとイメージできるところまではいってないんだよなー
半分機械的に書きなおしてみたら意外とうまくいった、て感じは拭えない

さてここまでわかればもう（やっと）two-in-a-row*?は目前です
ついでに第13の戒律を適用します

```
(define two-in-a-row*?
  (letrec
      ((T? (lambda (a)
             (let ((n (get-next (quote ()))))
               (if (atom? n)
                   (or (eq? n a) (T? n))
                   #f))))
       (get-next (lambda (x)
                   (let/cc here-again
                     (set! leave here-again)
                     (fill (quote go)))))
       (fill (lambda (x) x))
       (waddle (lambda (l)
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
       (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (let/cc here
                   (set! leave here)
                   (waddle l)
                   (leave (quote ())))))
        (if (atom? fst) (T? fst) #f)))))
```

本には(get-next 0)というのが1箇所だけ出てきますがなんですかね
たぶん書き間違いじゃないかと

さてこれって継続使わなかったらどう書けるんですかね
やってみましょう

あれ？
なんか難しい？
これは宿題！
