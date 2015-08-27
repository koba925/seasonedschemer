# Scheme修行(8) let

## 第14章 名前をつけましょう

この章ではletを覚えます。
letをletrecより後でとりあげたのはどういうことなんでしょう？

## コンフリクトの解消

訳あって13章と14章をそれぞれのブランチで並行して作業してたんですが、13章が終わったので14章のブランチに13章の内容をマージしようとしたところ

```
$ git co chap14
Switched to branch 'chap14'
$ git merge master
Auto-merging util.rkt
CONFLICT (content): Merge conflict in util.rkt
Automatic merge failed; fix conflicts and then commit the result.
```

出やがったなコンフリクト！

どれどれ。

```
<<<<<<< HEAD
(provide atom? add1 sub1 one? eqlist? pick member? Y)
=======
(provide atom? add1 sub1 one? pick member? reverse Y)
>>>>>>> master
```

ああそこね。修正。

```
(provide atom? add1 sub1 one? eqlist? pick member? reverse Y)
```

どれ。

```
$ git s
On branch chap14
You have unmerged paths.
  (fix conflicts and run "git commit")

Changes to be committed:

	new file:   blogs/seasonedschemer06.html
	new file:   blogs/seasonedschemer06.md
	new file:   blogs/seasonedschemer07.html
	new file:   blogs/seasonedschemer07.md
	modified:   chap12.rkt
	new file:   chap13.rkt

Unmerged paths:
  (use "git add <file>..." to mark resolution)

	both modified:   util.rkt

$ git d
diff --cc util.rkt
index 8b2a555,848411c..0000000
--- a/util.rkt
+++ b/util.rkt
@@@ -2,7 -2,7 +2,7 @@@
  
  (require rackunit)
  
- (provide atom? add1 sub1 one? eqlist? pick member? Y)
 -(provide atom? add1 sub1 one? pick member? reverse Y)
++(provide atom? add1 sub1 one? eqlist? pick member? reverse Y)
```
diffの結果はこんな風に表示されるのかー。

コンフリフトを解消したらaddしてcommitする、と。

```
$ git commit
[chap14 5a32265] Merge branch 'master' into chap14
$ git l5
*   5a32265 (HEAD -> chap14) Merge branch 'master' into chap14 - Takahiro ...
|\  
| * 142771f (tag: chap13, origin/master, master, chap13) 「Scheme修行(7) ...
| * 17ff612 rember-upto-lastのバリエーションを追加 - Takahiro Kobayashi, ...
| * 0cff06a 「Scheme修行(6) letcc登場」を追加 - Takahiro Kobayashi, 4 ...
| * faa8b64 intersectall hopを渡す版 - Takahiro Kobayashi, 4 days ago
```

準備完了。

## leftmost

一番左のアトムを探す`leftmost`です。

```
(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))
```

これは引数に空リストを含まないことが前提になっているので、空リストを含んでいてもよいようにしてやります。

```
(define leftmost2
  (lambda (l)
    (cond ((null? l) (quote ()))
          ((atom? (car l)) (car l))
          (else (cond
                  ((atom? (leftmost2 (car l)))
                   (leftmost2 (car l)))
                  (else (leftmost2 (cdr l))))))))
```

`(null? l)`だったときに`()`を返しているのは、たとえば`#f`を返すようにすると一番左のアトムが`#f`だった場合と区別がつかないからです。

`cond`の中の`((atom? (leftmost2 (car l))) (leftmost2 (car l)))`で、`(leftmost2 (car l))`を2回評価しているのがもったいないですね。

> そのような望ましくない繰り返しを防ぐために、`(letrec ...)`を用いてみましょう。
> 
> はい。でも、`(letrec ...)`で好きなものに名前をつけるのですか？
> 
> 代わりに`(let ...)`を使います。

というわけでこうなります。

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

わからないのはこれ。

> `(letrec ...)`と見かけは同じですが、`(let ...)`は式の値に名前をつけるのです。

`letrec`だって式の値に名前をつけてるんじゃないかと。
違いは名前の見えてるスコープだけだと思ってたんですが・・・

## rember1*

rember*はS式から指定したアトムをすべて取り除くものでしたが、最初に見つかったアトムだけを削除する関数を作ります。

```
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (cond
                   ((eqlist? (R (car l)) (car l))
                    (cons (car l) (R (cdr l))))
                   (else
                    (cons (R (car l)) (cdr l)))))))))
      (R l))))
```

`(eqlist? (R (car l)) (car l))`という条件は、「carにaが含まれていなければ」という意味ですね。

`(R (car l))`が2回呼ばれていますので`let`で書き直します。

```
(define rember1*3
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
:
:
                (else
                 (let ((av (R (car l))))
                   (cond
                     ((eqlist? av (car l))
                      (cons (car l) (R (cdr l))))
                     (else
                      (cons av (cdr l))))))))))
      (R l))))
```

## 第15の戒律(仮)

> 繰り返される式の値に名づくるには`(let ...)`を用うべし。

## depth*

リストの深さを求める`depth*`です。

```
(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth* (cdr l)))
          (else (cond ((> (depth* (cdr l))
                          (add1 (depth* (car l))))
                       (depth* (cdr l)))
                      (else
                       (add1 (depth* (car l)))))))))
```

`(depth* (cdr l))` と`(add1 (depth* (car l)))`が2回ずつ出てきます。

letで書きます。

```
(define depth*2
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*2 (cdr l)))
      (else
       (let ((a (add1 (depth*2 (car l))))
             (d (depth*2 (cdr l))))
         (cond ((> d a) d)
               (else a)))))))
```

よく見るとまだ`(depth* (cdr l))`が2回出てきてますね。こうすべきでしょうか？

```
(define depth*3
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
       (let ((d (depth*3 (cdr l))))
         (cond
           ((atom? (car l)) d)
           (else
            (let ((a (add1 (depth*3 (car l)))))
              (cond
                ((> d a) d)
                (else a))))))))))
```

こうすると確かに`(depth* (cdr l))`を繰り返し書かなくてすむようになりましたが、`(depth* (cdr l))`を評価する回数は実は変わっていません。
そのわりに、コードの方は`cond`と`let`の入れ子で読みづらくなっています。
ちょっとこれはやり過ぎだったようです。
DRY原則的には3つ目の方がよいところもありますが。

## 第15の戒律(改訂版)

> 関数定義において繰り返される式は、当の関数を1回使用するときに2回評価される可能性があるなら、
> それらの値を名付くるに`(let ...)`を用うべし。
