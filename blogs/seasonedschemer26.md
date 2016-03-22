# Scheme修行 (26) let/cc、残り、セルフ

先回りして書いてた*constについてはちょっとだけ
最初の *constの定義はこう

```
(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))
(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
         (car (cdr args-in-a-list))))))

(define *const
  (lambda (e table)
    (cond ...
          ((eq? e (quote cons)) (b-prim cons))
          ((eq? e (quote car)) (a-prim car))
          ...)))
```

これでも動くんですが、carとかconsを評価するたびに(b-prim cons)や
(a-prim car)を評価することになります

こう書きなおしておけば

```
(define *const
  (let ((:cons (b-prim cons))
        (:car (a-prim cons)))
    (lambda (e table)
      (cond ...
            ((eq? e (quote cons)) :cons)
            ((eq? e (quote car)) :car)
            ...))))
```

(b-prim cons)や(a-prim car)を評価するのは
defineのときの1回だけで済みます
letがlambdaの外側にあることが大事です
letが先に評価されるので、:consや:carを含むクロージャができます
letがlambdaの中にあると、lambdaを評価するたびに:consや:carを
全部作るのでかえって非効率になります

> 第15の戒律（最終版）
> 関数定義において繰り返される式は、当の関数を1回使用するときに2回評価される
> 可能性があるなら、それらの値を名づくるに(let ...)を用うべし。また、関数が
> 用いられるごとに再評価される(set!のない)式の値には、(let ...)を用いて
> 名づくるべし。

順番が逆になりましたが
a-primやb-primは、普通の関数を、*application向けに変換するものです
普通の関数は(f 'a 'b 'c)のように引数を取りますが
*appliationには(f ('a 'b 'c))のように引数をリストにして渡す必要があるためです

condはほぼ手習いの時と同じなので省略して*letccです

```
(define *letcc
  (lambda (e table)
    (let/cc skip
      (beglis (ccbody-of e)
              (extend (name-of e)
                      (box (a-prim skip))
                      table)))))
```

*lambdaと似てますね
*lambdaでは引数の名前と値（のbox）を組みにしていましたが
こちらでは継続の名前と継続を組にしています
a-primしているのはリストに入った引数をskipに渡すため
あとで(e '(result))としてやるとあたかもlet/ccがresultを返したかのように
動作を継続します

処理してるコードの中のlet/ccと、インタプリタの処理をlet/ccするのが
本当に一致するのかというとちょっと心配です
雰囲気的に動いてくれそうな感じではあるんですが

```
> (value '(let/cc hop 1))
1
> (value '(let/cc hop (hop 1) 2))
1
```

動いてはいますね
こんなのうまく追いかけられるかな

```
(value '(let/cc hop (hop 1) 2))
(the-meaning '(let/cc hop (hop 1) 2))
(meaning '(let/cc hop (hop 1) 2) lookup-in-global-table)
(*letcc '(let/cc hop (hop 1) 2) lookup-in-global-table)

(let/cc skip (beglis '((hop 1) 2)
                     (extend 'hop <skipの入ったbox> lookup-in-global-table)))
skip <- （REPLに値を返すだけの）継続
(beglis '((hop 1) 2) (extend 'hop <skipの入ったbox> lookup-in-global-table))
global-table -> | hop | <skipの入ったbox> |
((lambda (val) (beglis '(2) lookup-in-global-table))
   (meaning '(hop 1) lookup-in-global-table))
※以下(lambda(val) ...)は省略
(*application '(hop 1) lookup-in-global-table)
(*application '(hop 1) lookup-in-global-table)
((meaning 'hop lookup-in-global-table) (evlis '(1) lookup-in-global-table))
((*identifier 'hop lookup-in-global-table) '(1))
((unbox <skipの入ったbox>) '(1))
((a-prim skip) '(1))
(skip 1)
1
```

ちょっと例が簡単すぎたかな・・・
でも続きを実行してくれそうな気がしてきたぞ
「(REPLに値を返すだけの)」が「回りの式に値を返す」になるだけだもんね？

```
(value '(zero? (let/cc hop (hop 1) 2)))
```

だったら、えーと
これはイメージトレーングだけにとどめよう
`(hop 1)`を評価すると`(meaning '(let/cc hop (hop 1) 2))`の値が1になって
引き続き`(zero? 1)`を評価する感じになるはずだ
大丈夫だ

最後の仕上げです
the-empty-tableが半端なままです
こうなります
abort2は脱出用の継続です

```
(define the-empty-table
  (lambda (name)
    (abort2 (cons (quote no-answer) (cons name (quote ()))))))
```


そこに処理を書くのか
なんかすごいな
ほとんどそこにしか書くところはないけど

このthe-empty-tableが使えるよう、valueに手を加えます

```
(define value
  (lambda (e)
    (let/cc the-end
      (set! abort2 the-end)
      (cond ((define? e) (*define e))
            (else (the-meaning e))))))
```

```
> (the-empty-table 'z)
'(no-answer z)
> (value 'z)
'(no-answer z)
```

ちゃんと例外的な状況も処理できるようになりましたよと

さてこれでdefineもできるvalueができました
ということは、自分自身を実行することもできるはず

こんな感じで、すべての関数をvalueの中で改めて定義してやります

```
(value '(define value
          (lambda (e)
            (let/cc the-end
              (set! abort2 the-end)
              (cond ((define? e) (*define e))
                    (else (the-meaning e)))))))
```

もはやglobal-tableがどんなクロージャになっているのか想像するのも困難なレベル

動くかな・・・

```
> (value '(value (quote
                  (define length
                    (lambda (lat)
                      (cond ((null? lat) 0)
                            (else (add1 (length (cdr lat))))))))))
> (value '(value (quote (length (quote (a b c))))))
3
```

動いたー！
