# Scheme修行 (25) lambda、application

lambdaは手習いのvalueにもありましたが、set!が出てきた関係で
複数の式を書けるようにする必要があります

```
(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend (formals-of e)
                            (box-all args)
                            table)))))
```

クロージャの作られ方が手習いの時とすこし違うかな？
手習いの時はこうでした

```
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))
```

こちらは単純にテーブル・仮引数・関数本体をリストにして記憶してますね

今回は関数（クロージャ）を作って返し、あとでその関数を評価するようにしています
テーブル・仮引数・関数本体はクロージャに記憶されています
狙いはなんでしょうか

*applicationが出てきたらもう一度見てみます

あと、仮引数をboxに入れているのでdefineで作ったものと同じく
後から値を変更することが可能です

複数の式を処理するbeglisです
なんということはありません

```
(define beglis
  (lambda (es table)
    (cond ((null? (cdr? es)) (meaning (car es) table))
          (else ((lambda (val)
                   (beglis (cdr es) table))
                 (meaning (car es) table))))))
```

複数の式がある場合、途中の式の値は捨てられ、最後の式の値が返されます
なんか前の式の値が次の式に与えられるような雰囲気の書き方になってますが
捨てられるだけです
valに値を入れるけど実際には使わない、っていうちょっと変な書き方になってます
こういう書き方しかないのかな？

```
          (else (let ()
                   (meaning (car es) table)
                   (beglis (cdr es) table)))
```

とかさらに省いて

```
          (else
            (meaning (car es) table)
            (beglis (cdr es) table))
```

でもよさそうな気がしますけど

multi-extendはごく普通に書いてあるだけなので省略
lambdaを評価してみます

```
> (value '(lambda (x) x))
#<procedure:...hemer/chap20.rkt:254:4>
```

\#\<procedure:... というのは値が関数（クロージャ）ですよ、と言ってます
流れの確認

```
(value '(lambda (x) x))
(the-meaning '(lambda (x) x))
(meaning '(lambda (x) x) lookup-in-global-table)
((expression-to-action '(lambda (x) x)) '(lambda (x) x) lookup-in-global-table)
(*lambda  '(lambda (x) x) lookup-in-global-table)
(lambda (args)
  (beglis '(x)
    (multi-extend '(x) (box-all args) lookup-in-global-table)))
```

lambdaだけあってもしかたがないので*applicationを作ります

```
(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))
```

ややこしいことやってるはずな割には短くて簡潔の見本みたいなコードですね
手習いだとここが相当しそうです

```
(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
           (evlis (arguments-of e) table))))
           
(define apply
  (lambda (fun vals)
    (cond ((primitive? fun) (apply-primitive (second fun) vals))
          ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
                           (table-of closure)))))
```

*lambdaと組み合わせて見てみると
applyやapply-closureがなくなってしまっていることがわかります
プリミティブとlambdaを区別せずに評価できるようになってるということですね

手習いの時は、lambdaを評価したときの値は
`(non-primitive テーブル 仮引数 本体)`というリストで、
これがクロージャを表していることになっていました
クロージャの内容も表示することができたので、クロージャの正体をつかみやすかった記憶があります
ただ、これはただのリストですのでそのまま評価することはできず、
apply-closureでリストを分解してあらためて評価してやる必要がありました

今回はクロージャを表すリストでなく、このようなlambdaといっしょにlambdaを評価した時点の
eとかtableとかを記憶している、クロージャそのものを持っています

```
(lambda (args)
  (beglis (body-of e)
          (multi-extend (formals-of e)
                        (box-all args)
                        table))
```

そのため、*applicationで引数を直接あたえて評価することができるようになったんですね
結局手習いの時と同じ動きになっています
プリミティブな関数も同様のしくみで実現するようになっているので
*application以下の関数に場合分けの必要もなくなり、今のようにシンプルにできた、と
*lambdaのところで気になった「狙い」はそういうことだったと思われます

evlisも普通っちゃあ普通

```
(define evlis
  (lambda (args table)
    (cond ((null? args) (quote ()))
          (else ((lambda (val)
                   (cons val (evlis (cdr args) table)))
                 (meaning (car args) table))))))
```

さっきの*lambdaにもほとんど同じような書き方が出てきましたが
今度はvalは捨てられずに使われています
健全

「ここでの定義がSchemeで常に動作するようにそうしています」という注がついています
ということは

```
          (else (let ()
                   (meaning (car es) table)
                   (beglis (cdr es) table)))
```

みたいな書き方を許さないSchemeがあるということかな？

あらためて考えてみると、(lambda () ...)の「...」に複数の式を並べて
書けなくてもlambdaの入れ子を増やしていけば同じ意味の式になるんですね
これもシンタックスシュガーだったのか

さて*lambda と *application が書けましたので関数を評価できるようになりました

```
> (value '((lambda (x) x) 1))
1
```

追います

```
global-table -> 空

(value '((lambda (x) x) 1))
(the-meaning '((lambda (x) x) 1))
(meaning '((lambda (x) x) 1) lookup-in-global-table)
((expression-to-action '((lambda (x) x) 1)) 
 '((lambda (x) x) 1) lookup-in-global-table)
(*application  '((lambda (x) x) 1) lookup-in-global-table)
((meaning  '(lambda (x) x) lookup-in-global-table)
 (evlis '(1) lookup-in-global-table))
((lambda (args)
   (beglis '(x)
     (multi-extend '(x) (box-all args) lookup-in-global-table)))
 '(1))
(beglis '(x)
     (multi-extend '(x) (box-all '(1)) lookup-in-global-table)))

global-table -> | x | 1 |

(meaning 'x lookup-in-global-table)
(*identifier 'x lookup-in-global-table)
1
```

defineと組み合わせることもできます

```
> (value '(define id (lambda (x) x)))
> (value '(id 2))
2
```

追います

```
global-table -> 空

(value '(define id (lambda (x) x)))
(*define '(define id (lambda (x) x)))
(set! global-table (extend 'id
                           (box (the-meaning (lambda (x) x)))
                           global-table))

global-table -> | id | (lambda (args) (beglis '(x) ...) |

(value '(id 2))
(meaning '(id 2) lookup-in-global-table)
(*application '(id 2) lookup-in-global-table)
((meaning 'id lookup-in-global-table) (evlis '(2) lookup-in-global-table))
((lambda (args)
   (beglis '(x)
     (multi-extend '(x) (box-all args) lookup-in-global-table)))
 '(2))

global-table -> | id | (lambda (args) (beglis '(x) ...) |
                | x  | 2                                |

(meaning 'x lookup-in-global-table)
(*identifier 'x lookup-in-global-table)
2
```

lambdaが評価できるようになったということはtrueとfalseとifが定義できるように
なったということですね！

```
> (value '(define otrue (lambda (x y) x)))
> (value '(define ofalse (lambda (x y) y)))
> (value '(define oif (lambda (cnd thn els) (cnd thn els))))
> (value '(oif otrue 1 2))
1
> (value '(oif ofalse 1 2))
2
```

これでcondは不要です（嘘とも言い切れない

