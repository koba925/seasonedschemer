# Scheme修行 (28) マクロ

> ここまでやったからには簡単でいいからマクロとして実装してみたいですね

scheme本来のマクロはけっこうややこしい感じだし原理を確認したいだけだから
うんと単純なやつで

\*letと\*letrecをじっと見ると、eを変換してからmeaningにかければいいっぽい

eを変換するのにもvalueのしくみ自身を使うわけですけど
今のママ(value '(lambda ...) e)とやっても望みの結果は得られません
*applicationに似てるけど、引数をevlisせずそのまま渡して、返された値をもう一度
評価する関数がいるはず
こういう感じ

```
(define *macro-application
  (lambda (e table)
    (meaning (expand e table) table)))
(define expand
  (lambda (e table)
    ((meaning (function-of e) table)
              (arguments-of e))))
```

関数をふたつに分けているのは、expandだけ呼んで正しくマクロが展開されているか
確認できるようにしたかっただけです

・・・ということは
式を見て\*applicationを呼ぶか\*macro-applicationを呼ぶか見分けがつかないといけません
どうやって区別しよう？

手習い式のクロージャだったらただのリストなのでnon-primitiveの代わりにmacroとか
書いてれば済んだんでしょうが今や本当のクロージャそのものだから・・・
うまい情報のもたせ方あるかなあ？

kons式にセレクタを作って、関数なのかマクロなのかを返してくれるようにする手はあるな
\*lambdaと\*applicationも修正しないといけないけど

と思って考えてみたけどあんまりいい感じじゃない

あーあれか？
種類を持たせるなんてけちくさいことを言わず、\*applicationそのものを
覚えさせておけばいいか？できるか？
これができればかっこいい気がする
Felleisen先生の意図どおりかもしれない？

プリミティブなやつもこれに合わせないとなのか
a-primとかb-primとかに吸収できそうではあるけど
なんか大げさだなあ

なんかもっとこじんまりしないかな
難しいことせずにマクロ用のテーブルを作って分けちゃうか
マクロの名前のほうが優先して検索されちゃうけどscheme的にはどうなんだろうな

```
(define macro-table (lambda (name) #f))
(define macro?
  (lambda (e)
    (lookup macro-table e)))
```

見つからなかったら継続とかすごいことせずに#fを返すようにしておきます
見つかればマクロ（に相当するlambda）を返すはずなので区別はつくはず

マクロを定義するところ
構文は(defmacro <マクロ名> (lambda ...))と考えてます
マクロ定義に対してset!することはないことにしてboxは省略

```
(define value
  (lambda (e)
    (let/cc the-end
      (set! abort2 the-end)
      (cond ((define? e) (*define e))
            ((defmac? e) (*defmac e)) ; ここ追加
            (else (the-meaning e))))))

(define defmac?
  (lambda (e)
    (cond ((atom? e) #f)
          ((atom? (car e)) (eq? (car e) (quote defmac)))
          (else #f))))

(define *defmac
  (lambda (e)
    (set! macro-table
          (extend (name-of e)
                  (the-meaning (right-side-of e))
                  macro-table))))
```

式を見て関数かマクロか決めるところ

```
(define list-to-action
  (lambda (e)
    (cond ((atom? (car e))
           (cond ...
                 ((macro? (car e)) *macro-application) ; ここ追加
                 (else *application)))
          (else *application))))
```

それから、*identifierは両方のテーブルを探すようにします

```
(define *identifier
  (lambda (e table)
    (let ((m (lookup macro-table e)))
      (cond ((eq? m #f) (unbox (lookup table e)))
            (else m)))))
```

できたぽいです
まずは簡単なやつで試してみます

```
> (value '(defmac set1
            (lambda (name)
              (cons (quote set!)
                    (cons name
                          (cons 1 (quote ())))))))
> (expand '(set1 a) lookup-in-global-table)
'(set! a 1)
> (value '(define a 0))
> (value 'a)
0
> (value '(set1 a))
> (value 'a)
1
```

できました
ほんとはそんなにすんなりできたわけじゃありません

ではletやってみます

・・・

letって引数が可変長じゃないか
まあ可変長受け取れるように作ればいいんだけど
可変長なところはカッコでくくることにしちゃおう
束縛するところはカッコでくくるんだから本体をカッコでくくっちゃだめという法はない（開き直り
書く気になれば書けると思うから！

```
(value '(define let-formals-of
          (lambda (binds)
            (cond ((null? binds) (quote ()))
                  (else (cons (car (car binds))
                              (let-formals-of (cdr binds))))))))
(value '(define let-args-of
          (lambda (binds)
            (cond ((null? binds) (quote ()))
                  (else (cons (car (cdr (car binds)))
                              (let-args-of (cdr binds))))))))
(value '(defmac my-let
          (lambda (binds body)
            (cons (cons (quote lambda)
                        (cons (let-formals-of binds) body))
                  (let-args-of binds)))))
```

まずは正しく展開されるか確かめてみよう

```
> (expand '(my-let
             ((x (quote a))
              (y (cons (quote b) (quote ()))))
             ((cons x y))) ; ここのカッコがひとつ多い
          lookup-in-global-table)
'((lambda (x y) (cons x y)) 'a (cons 'b '()))
```

OK
では実行

```
> (value '(my-let
            ((x (quote a))
             (y (cons (quote b) (quote ()))))
            ((cons x y))))
'(a b)
```

おｋ
ふー終わり終わり
letrecは新しいシンボル作るところがめんどっちいからパス

あ、でも「普通の」defineも書けるようにしてみたかったんだった
(define (add2 x) (add1 (add1 x)))みたいに書くやつね
本体に複数の関数を書けるようにはしないよ！

```
> (value '(defmac my-define
            (lambda (form body)
              (cons 'define
                    (cons (car form)
                          (cons (cons 'lambda
                                      (cons (cdr form)
                                            (cons body (quote ()))))
                                (quote ())))))))
> (expand '(my-define (add2 x) (add1 (add1 x))) lookup-in-global-table)
'(define add2 (lambda (x) (add1 (add1 x))))
```

よしよし
では

```
> (value '(my-define (add2 x) (add1 (add1 x))))
'(no-answer define)
```

あり？

ああ、meaningにはdefineないもんね
マクロで変換するところはvalueを呼ぶほうがよかったのかな？
つまりこう？

```
(define *macro-application
  (lambda (e table)
    (value (expand e table))))
```

add2は動いた

```
> (value '(my-define (add2 x) (add1 (add1 x))))
> (value '(add2 1))
3
```

けどこれじゃ変換後の式の評価でtableに入ってる環境が使われないからおかしくない？
なんか沼にはまってる？

どんなときにおかしくなるかな
マクロ内で自由変数を参照してる時とかか

```
> (value '(my-let ((x (quote a)))
                  ((my-let ((y (cons (quote b) (quote ()))))
                            ((cons x y))))))
'(no-answer x)
```

ほらね
じゃねーよ

tableで環境を引き継がなきゃいけないとすると、*macro-applicationは元に戻すとして、
valueじゃなくてmeaningでdefineを扱えるようにする必要がある
これはまあやるだけっちゃあやるだけでできそうなんだけど
なぜdefineをvalueに置いておいたかっていうのが問題だ

とりあえずやるだけっていうのはこういうこと

```
(define value
  (lambda (e)
    (let/cc the-end
      (set! abort2 the-end)
      (the-meaning e))))

(define meaning
  (lambda (e table)
    (cond ((define? e) (*define e)) ; ここへ移動
          ((defmac? e) (*defmac e)) ; これも
          (else ((expression-to-action e) e table)))))
```

これでmy-defineもさっきのmy-letも両方動く
しかし意味もなくdefineをvalueで扱うようになっていたはずもない

valueで扱うようにしたということは、(value '(define ...))の形しか許さないということ
meaningでdefineを扱うようにしてしまうと式の途中のdefineまで処理しようとしてしまう

それで何がまずいのかというと
こんな感じで一見局所的な名前が定義できたように見えても

```
> (value '((lambda (x)
             (define temp 3)
             (eq? temp x))
           4))
#f
```

実は局所的じゃなかったとか

```
> (value 'temp)
3
```

そんなことかな
scheme準拠ならこういうこともできなきゃいけないはずなんだけど
これをなんとかするのはちょっと大変そうだ
冷静に考えるとmy-defineをあきらめるくらいが相場？
今回はこれで終わりにしておこう

またいつか