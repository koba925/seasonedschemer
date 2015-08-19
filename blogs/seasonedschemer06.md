# Scheme修行(6) letcc登場

## 第13章 ホップ、スキップ、ジャンプ

継続が出てきます。

## クローン

この章では`member?`を使います。
`member?`は12章でも定義しましたが、あらためてutil.rktに定義を入れて使おうと思います。
淡々と修正してもいいんですが、別の担当者のつもりでリポジトリをクローンしてみます。
ひとつ上のディレクトリに移動し、seasonedschemer-utilというディレクトリにリポジトリをクローンします。

```
$ cd ..
$ git clone https://github.com/koba925/seasonedschemer seasonedschemer-util
Cloning into 'seasonedschemer-util'...
remote: Counting objects: 95, done.
remote: Compressing objects: 100% (42/42), done.
remote: Total 95 (delta 22), reused 0 (delta 0), pack-reused 52
Unpacking objects: 100% (95/95), done.
Checking connectivity... done.
$ cd seasonedschemer-util/
$ ls -a
.		.git		blogs		chap12.rkt
..		.gitignore	chap11.rkt	util.rkt
$ git l
* f722ae7 (HEAD -> master, origin/master, origin/HEAD) 「Scheme修行(5) 第12章 残り & 第13の戒律」を追加 - Takahiro Kobayashi, 25 hours ago
* 4ebdb46 「Scheme修行(4) 第12章 multirember & 第12の戒律」を追加 - Takahiro Kobayashi, 25 hours ago
* 5e2134b ev?、od?、sum-of-prefixes、scrambleを追加 - Takahiro Kobayashi, 25 hours ago
```

できました。util.rktを修正してコミットします。ブランチ作成は省略してmasterで作業しました。

```
$ git s
On branch master
Your branch is up-to-date with 'origin/master'.
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

	modified:   util.rkt
$ git ac
[master 1f9de67] util.rktにmember?を追加
 1 file changed, 16 insertions(+), 3 deletions(-)
$ git l
* 1f9de67 (HEAD -> master) util.rktにmember?を追加 - Takahiro Kobayashi, 37 seconds ago
* f722ae7 (origin/master, origin/HEAD) 「Scheme修行(5) 第12章 残り & 第13の戒律」を追加 - Takahiro Kobayashi, 25 hours ago
* 4ebdb46 「Scheme修行(4) 第12章 multirember & 第12の戒律」を追加 - Takahiro Kobayashi, 25 hours ago
```

リモートにpushします。クローンしたのでリモートは指定不要。

```
$ git push
Counting objects: 3, done.
Delta compression using up to 4 threads.
Compressing objects: 100% (3/3), done.
Writing objects: 100% (3/3), 479 bytes | 0 bytes/s, done.
Total 3 (delta 2), reused 0 (delta 0)
To https://github.com/koba925/seasonedschemer
   f722ae7..1f9de67  master -> master
$ git l
* 1f9de67 (HEAD -> master, origin/master, origin/HEAD) util.rktにmember?を追加 - Takahiro Kobayashi, 2 minutes ago
* f722ae7 「Scheme修行(5) 第12章 残り & 第13の戒律」を追加 - Takahiro Kobayashi, 25 hours ago
* 4ebdb46 「Scheme修行(4) 第12章 multirember & 第12の戒律」を追加 - Takahiro Kobayashi, 25 hours ago
```

seasonedschemerディレクトリに戻ってgit fetchでリモートから修正を持ってきます。

```
$ cd ..
$ cd seasonedschemer
$ git fetch
remote: Counting objects: 3, done.
remote: Compressing objects: 100% (1/1), done.
remote: Total 3 (delta 2), reused 3 (delta 2), pack-reused 0
Unpacking objects: 100% (3/3), done.
From https://github.com/koba925/seasonedschemer
   f722ae7..1f9de67  master     -> origin/master
$ git l
* 1f9de67 (origin/master) util.rktにmember?を追加 - Takahiro Kobayashi, 3 minutes ago
* f722ae7 (HEAD -> chap13, tag: chap12, master) 「Scheme修行(5) 第12章 残り & 第13の戒律」を追加 - Takahiro Kobayashi, 25 hours ago
* 4ebdb46 「Scheme修行(4) 第12章 multirember & 第12の戒律」を追加 - Takahiro Kobayashi, 25 hours ago
```

masterにマージします。

```
$ git co master
Switched to branch 'master'
Your branch is behind 'origin/master' by 1 commit, and can be fast-forwarded.
  (use "git pull" to update your local branch)
$ git merge
Updating f722ae7..1f9de67
Fast-forward
 util.rkt | 19 ++++++++++++++++---
 1 file changed, 16 insertions(+), 3 deletions(-)
```

git pullとやると、fetchしてmergeしてというのを一度にやってくれるそうですが、fetchしてmergeするほうがおすすめだそうです。
まだわかっていない部分も多いので盲目的に従っておきます。

これで`member?`が使えるようになりました。
最低限だとこんなところでしょうか？

## intersectall

題材は`intersectall`です。まずは`intersect`から。

```
(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) (quote ()))
          ((member? (car set1) set2)
           (cons (car set1) (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))
```

第12の戒律を使って書き換えます。

```
(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond ((null? set) (quote ()))
                    ((member? (car set) set2)
                     (cons (car set) (I (cdr set))))
                    (else (I (cdr set)))))))
      (I set1))))
```

## 過去のバージョンのファイルを見る

最初のバージョンの`intersect`を書いてコミットして書き換えてコミットして、とやると両方見たいときには少し不便です。
古いバージョンを残したまま、あるいはコメントアウトして残しておくほうがお勉強中は便利かもしれません。
書き換えてしまった部分を表示するには、git showでコミットとファイルを指定してやればいいようです。

```
$ git show 8f0db0b:chap13.rkt
:
:
(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) (quote ()))
          ((member? (car set1) set2)
           (cons (car set1) (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))
:
:
$ git show 4a3f572:chap13.rkt
:
:
(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond ((null? set) (quote ()))
                    ((member? (car set) set2)
                     (cons (car set) (I (cdr set))))
                    (else (I (cdr set)))))))
      (I set1))))
:
:
```

過去のファイルを指定して差分を表示することもできます。このへんを駆使すれば勉強もOKかなあ。

```
$ git d 8f0db0b:chap13.rkt 4a3f572:chap13.rkt
diff --git a/8f0db0b:chap13.rkt b/4a3f572:chap13.rkt
index ecf98ab..166bc2c 100644
--- a/8f0db0b:chap13.rkt
+++ b/4a3f572:chap13.rkt
@@ -5,10 +5,13 @@

 (define intersect
   (lambda (set1 set2)
-    (cond ((null? set1) (quote ()))
-          ((member? (car set1) set2)
-           (cons (car set1) (intersect (cdr set1) set2)))
-          (else (intersect (cdr set1) set2)))))
+    (letrec
+        ((I (lambda (set)
+              (cond ((null? set) (quote ()))
+                    ((member? (car set) set2)
+                     (cons (car set) (I (cdr set))))
+                    (else (I (cdr set)))))))
+      (I set1))))
```

## intersectall

以前に書いたバージョンです。

```
(define intersectall
  (lambda (lset)
    (cond ((null? (cdr lset)) (car lset))
          (else (intersect (car lset)
                           (intersectall (cdr lset)))))))
```

このバージョンは、`lset`が`null?`でないという前提がありました。`null?`でもよいように書き換えます。

```
(define intersectall
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond ((null? (cdr lset)) (car lset))
                    (else (intersect (car lset)
                                     (A (cdr lset))))))))
      (cond ((null? lset) (quote ()))
            (else (A lset))))))
```

## lambdaとletrecの違い

> これ(`letrec`のこと)は`(lambda (x y) M)`と似ていますよね。
> 
> はい、そうです。名前`x`と`y`は、`M`が何であろうと、`M`の内部でのみ有効です。そして  
`(letrec ((x F) (y G)) M)`  
> では、名前`x`と`y`は、`F`と`G`と`M`が何であろうと、`F`と`G`と`M`の内部でのみ有効です。

## letcc

`(inersectall '((3 mangoes and) () (3 diet hamburgers))`の値は`()`になります。`()`との共通部分を取っているので当然ですが、`intersectall`は正直に全部を`intersect`し続けます。
`()`と出会ったらすぐ、今までやってきたことを放棄して`()`を返すようにします。

というところで`letcc`の登場です。ただし、Racketでは`letcc`ではなく`let/cc`になっています。

```
(define intersectall
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond ((null? (car lset))
                       (hop (quote ())))
                      ((null? (cdr lset)) (car lset))
                      (else (intersect (car lset)
                                       (A (cdr lset))))))))
        (cond ((null? lset) (quote()))
              (else (A lset)))))))
```

`A`の中で`(null? (car lset))`だった場合に`(hop (quote ())`を評価します。このとき何が起こるかというと

> `(letcc hop`を通り過ぎた後、`(hop M)`に遭遇する前にすべきだったことを忘れなさい。そして、`M`が何であっても、まるで`(letcc hop M)`の値を求めたかのようにふるまいなさい。

ということだそうです。
この文章がいまひとつ飲み込めてなかったりします。

* `(hop M)`の`M`と`(letcc hop M)`の`M`は同じなの？  
コードと対応付けてみると`(hop M)`の`M`は`(quote ())`で、`(letcc hop M)`の`M`は`(letrec ...)`に見えます。そうじゃないとすれば両方`(quote ())`ってことなんでしょうけど。
* `(letcc hop M)`の値を求めたかのようにふるまいなさいと言われても？  
値は何なんですか？`M`なんでしょうけどそれなら「まるで`(letcc hop M)`の値は`M`であるかのように」と書けばよさそうなものですが・・・

引っ掛かりはするものの、`(letcc hop M)`の値は`M`という理解で進みます。
この評価が、実際に関数と同じ仕組みで行われているかはわかりませんが、同じように考えても問題はなさそうです。
ひさしぶりに1ステップずつ評価を追ってみます。上の規則を踏まえるとこんな感じでしょうか。

```
(intersectall-c '((a b c) () (b e c)))
(let/cc hop (A '((a b c) () (b e c))))
(let/cc hop (intersect '(a b c) (A '(() (b e c)))))
(let/cc hop (intersect '(a b c) (hop (quote ()))))
(let/cc hop (quote ()))
(quote ())
```

## Alonzo Churchなら？

> Alonzo Church (1903-1995) ならこう書いたでしょう。
> 
> ```
>   :
>   (call-with-current-continuation
>     (lambda (hop)
>   :
> ```

なんだそうですが、Churchさんはコンピュータもない時代に継続まで考えてたってことでしょうか？
どういう思考回路？と思いましたがさらっと検索した感じだと別にChurchが継続を発明したというわけではなさそうです。

とすると何が言いたかったのか？

## 第14の戒律

> `(letcc ...)`を用いて、値を直ちに、すばやく返すべし。

## さらにletcc

`lset`に`()`が含まれていなくても、`intersect`した結果が`()`であれば`intersectall`の結果も`()`になります。その場合もすぐに結果を返すようにしたいものです。
`intersect`を内部関数にして`hop`を共有できるようにし、`intersect`の処理から直接`intersectall`の結果を返すようにします。

```
(define intersectall
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond ((null? (car lset))
                       (hop (quote ())))
                      ((null? (cdr lset)) (car lset))
                      (else (I (car lset) (A (cdr lset)))))))
           (I (lambda (set1 set2)
                (letrec
                    ((J (lambda (set1)
                          (cond ((null? set1) (quote ()))
                                ((member? (car set1) set2)
                                 (cons (car set1) (J (cdr set1))))
                                (else (J (cdr set1)))))))
                  (cond ((null? set2) (hop (quote ())))
                        (else (J set1)))))))
        (cond ((null? lset) (quote()))
              (else (A lset)))))))
```

実行の様子。

```
(intersectall '((a b) (c d) (e f) (g h)))
(let/cc hop (A '((a b) (c d) (e f) (g h))))
(let/cc hop (I '(a b) (A '((c d) (e f) (g h)))))
(let/cc hop (I '(a b) (I '(c d) (A '((e f) (g h))))))
(let/cc hop (I '(a b) (I '(c d) (I '(e f) (A '((g h)))))))
(let/cc hop (I '(a b) (I '(c d) (I '(e f) '(g h)))))
(let/cc hop (I '(a b) (I '(c d) '())))
(let/cc hop (I '(a b) (hop (quote ()))))
(let/cc hop (quote ()))
(quote ())
```

こういうのが思ったとおりに動いているかどうかは、こういうテストではわかりませんね。

```
(check-equal? (intersectall '((a b) (c d) (e f) (g h))) '())
```

なにかいい方法はあるんでしょうか。

ファーストクラスの継続、と言うくらいですから内部関数にする代わりに`hop`を渡すようにしても動くでしょうか？

```
(define intersect2
  (lambda (set1 set2 hop)
    (letrec
        ((I (lambda (set)
              (cond ((null? set) (quote ()))
                    ((member? (car set) set2)
                     (cons (car set) (I (cdr set))))
                    (else (I (cdr set)))))))
      (cond ((null? set2) (hop (quote ())))
            (else (I set1))))))

(define intersectall2
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond ((null? (car lset))
                       (hop (quote ())))
                      ((null? (cdr lset)) (car lset))
                      (else (intersect2 (car lset) (A (cdr lset)) hop))))))
        (cond ((null? lset) (quote()))
              (else (A lset)))))))
```

動きました。

## 今回覚えたgitコマンド
* git clone
* git fetch
* git pull
* git show <コミット>:<ファイル>
* git diff <コミット>:<ファイル> <コミット>:<ファイル>