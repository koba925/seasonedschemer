# Scheme修行(2) はじめに ＆ two-in-a-row?

##はじめに

「訳者まえがき」や「はじめに」はだいたいScheme手習いと似たようなことが書いてあるのでさらっといきます。

* 「継続(continuation)と代入(set!)という新たな概念を使用してプログラミングの幅を広げている」
継続はともかくとして、代入なんて、と思ってると足元をすくわれそうな予感がします。

* 「この本の目的は、読者に計算の性質について考えることを教えることにある」  
手習いでは「この本の目的は、読者に再帰的に考えることを教えることにある」と書いてありました。
「計算の性質」とはなんでしょうか？

* 「この本を5回未満で読み切ろうとしないこと」  
手習いでは「この本を2回以下で読み切ろうとしないこと」。
最後まで読むのを５回繰り返せと言っているんでしょうか、4回挫折してもいいから5回目まであきらめずに読め、って言っているんでしょうか。

## 第11章 おかえりなさい、ようこそショーへ

Scheme手習いはそこそこ読んでからブログ書き始めたので大体の流れがわかっていたんですが、Scheme修行はまだほとんど読んでません。
なので、11章はこれこれをやります、みたいなことはよくわかってません。
これから何をやるのか書いてないのは相変わらずです。
11章を読み終わってから考えます。

## two-in-a-row?

ラットの中に同じ要素が続けて２回出てきたら#tを返す、`two-in-a-row?`を作ります。

### ユニットテスト

テストを書いても成功だと何も出力されなくてほんとに実行してるか不安なので、いったん全部失敗させるといいかなと思いました。
もしかしたら関数が定義されていなくてもエラーを補足してテストを続行してくれるんじゃないかと期待してテストだけ書いてみます。

two-in-a-row.rkt

```
#lang racket/base

(require rackunit)
(require "util.rkt")

(check-false (two-in-a-row? '()))
(check-false (two-in-a-row? '(a)))
(check-true (two-in-a-row? '(a a)))
(check-true (two-in-a-row? '(a a b)))
(check-true (two-in-a-row? '(a b b)))
(check-false (two-in-a-row? '(a b c)))
```

実行。だめでした。

```
two-in-a-row?: unbound identifier in module in: two-in-a-row?
```

でもまともに定義するとテストが失敗しないしなあ。
（実は`check-true`は#t「だけ」しか成功しないので、この場合は手があるんですが）

テストがちゃんと動いてるかっていうのはどうやって確かめるんでしょうね？
成功でも何か出力するようにはできるのかな？それはそれでウザいことになりそうです。
成功の数を出力するくらいがいいバランス？やりかたはあるかな？

### two-in-a-row? 第1弾

あきらめて普通に関数を書きます。まずはちらっと読んで自分で書いてみます。

two-in-a-row.rkt(抜粋)

```
(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          ((is-first? (car lat) (cdr lat)) #t)
          (else (two-in-a-row? (cdr lat))))))

(define is-first?
  (lambda (a lat)
    (cond ((null? lat) #f)
          ((eq? (car lat) a) #t)
          (else #f))))
```

動きました。何も出力されません。不安です。

### 差分

前回、差分を確認するっていうのをやってなかったので試してみます。

```
$ git diff
$ git add .
$ git diff
$
```

何も出力されませんね。ファイルを追加したばっかりはそんなものなんでしょう。
練習のため、コミットしてから本のソースのとおりに修正します。
`or`の使い方とかいいですね。

```
(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (or (is-first? (car lat) (cdr lat))
                    (two-in-a-row? (cdr lat)))))))

(define is-first?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (eq? (car lat) a)))))
```

実行しても何も出力されません。不（略

差分は今度こそ出るでしょう。

```
$ git diff
diff --git a/two-in-a-row.rkt b/two-in-a-row.rkt
index 63f240c..28a44c6 100644
--- a/two-in-a-row.rkt
+++ b/two-in-a-row.rkt
@@ -6,14 +6,13 @@
 (define two-in-a-row?
   (lambda (lat)
     (cond ((null? lat) #f)
-          ((is-first? (car lat) (cdr lat)) #t)
-          (else (two-in-a-row? (cdr lat))))))
+          (else (or (is-first? (car lat) (cdr lat))
+                    (two-in-a-row? (cdr lat)))))))
 
 (define is-first?
   (lambda (a lat)
     (cond ((null? lat) #f)
-          ((eq? (car lat) a) #t)
-          (else #f))))
+          (else (eq? (car lat) a)))))
 
 (check-false (two-in-a-row? '()))
 (check-false (two-in-a-row? '(a)))
```

出ました出ました。
ファイルをステージするとgit diffの対象外になるようで、差分が出なくなります。
なぜそういう仕様なのか、今ひとつピンときません。
ということは、git diffの使いどころがピンときてないってことなんでしょう。

### ステージして同時にコミット

コミットします。addしてcommitするのも飽きてきました。嘘です。addするの忘れてcommitしたので`no changes added to commit`と怒られました。
commitにオプションをつけるとaddしてからcommitしてくれるようです。

```
$ git help commit
:
        4. by using the -a switch with the commit command to automatically
           "add" changes from all known files (i.e. all files that are already
           listed in the index) and to automatically "rm" files in the index
           that have been removed from the working tree, and then perform the
           actual commit;
:
```

やってみました。

```
$ git commit -a
[master 6e1114e] two-in-a-row?とis-first?を修正 テキストの通りに修正した
 1 file changed, 3 insertions(+), 4 deletions(-)
latour:seasonedschemer takahiro$ git diff
```
成功です。差分も出なくなりました。

### ブランチ

> 同じことをするのに別の方法があります。

これはブランチの試しどころ？
普通に改善するだけならそのまま続けるだけのような気がしますけどどうなんでしょう。
うまくいくかどうかわからないけどやってみる、みたいなシチュエーションだと思えばいいでしょうか。
いや、やっぱりmasterは手を付けず、なんにしろ修正するときはまずブランチを切ってからやったほうがいいのか。まあここは練習なので。

まずは軽くブランチについてお勉強。

* ブランチはコミットへのポインタ
* HEADブランチは、現在利用中のブランチを指す特殊なポインタ  
ちなみにmasterブランチは特別なブランチではなくて、たまたまgit initがそういう名前で作るだけの普通のブランチ
* チェックアウトすると、指定したブランチが利用中のブランチになり、ファイルもそのブランチが指しているファイルに差し替わる
* コミットすると、現在のブランチが今コミットしたスナップショットを指すようになる
* HEADもいっしょについてくる

ブランチを切る前に、すっきりした状態であることを確認しておきます。

```
$ git status
On branch master
nothing to commit, working directory clean
$ git diff
$
```

git logにブランチがわかりやすく表示されるオプションがあるので試してみます。

```
$ git log --oneline --decorate --graph --all
* f763eb5 (HEAD -> master) 答え合わせ版
* 9eeaf5e two-in-a-row(自分で書いた版)を追加
* a575d5d (origin/master) 「Scheme修行(1) 始める前に」を追加
* 398b4e2 最初のコミット
```

--onelineは1行表示、--decorateはブランチの表示、--graphはグラフ表示、--allは・・・何を全部なんだろう？今も別に全部表示されてますけど。-allを取っても表示はかわりません。
今のところmasterしかブランチがないせいでしょうか、グラフ表示はありません。
HEADとmasterブランチがどこを指しているか確認しておきます。

ブランチを切ります。

```
$ git branch newtir
```

newtirというのはnew-two-in-a-rowのつもり。どんな名前を付けるのがいいんでしょう？たくさんブランチを切ってたら名前がネタ切れしそうです。
これは新たにブランチ、つまりポインタを作るだけです。

```
$ git log --oneline --decorate --graph
* f763eb5 (HEAD -> master, newtir) 答え合わせ版
* 9eeaf5e two-in-a-row(自分で書いた版)を追加
* a575d5d (origin/master) 「Scheme修行(1) 始める前に」を追加
* 398b4e2 最初のコミット
```

あたらしくnewtirブランチができていること、HEADはまだmasterを指していることがわかります。

カレントのブランチを切り替えます。

```
$ git checkout newtir
Switched to branch 'newtir'
```

確認します。

```
$ git log --oneline --decorate --graph
* f763eb5 (HEAD -> newtir, master) 答え合わせ版
* 9eeaf5e two-in-a-row(自分で書いた版)を追加
* a575d5d (origin/master) 「Scheme修行(1) 始める前に」を追加
* 398b4e2 最初のコミット
```

HEADがnewtirを指すようになりました。

### two-in-a-row? 第2弾

scheme修行の内容に戻ります。

> `(is-first?)`が`#f`と答える場合が2通りあるのは本当ですか。
> はい。`lat`が空の時、または、リストの最初の要素が`a`ではないときに`#f`を返します。

`lat`が空であるというチェックが、`two-in-a-row?`と`is-first`の両方で行われていて重複してるからなんとかしたい、ということでしょうか。
テキストにはまずこういう修正版が出てきます。

two-in-a-row.rkt(抜粋)

```
(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (two-in-a-row? lat))))))
```

テストは通りますが、`lat`が空であるというチェックは相変わらず重複しています。
`two-in-a-row?`と`is-first`が相互に再帰するようになったというところがちょっとおもしろいところではありますが、具体的に何かよくなったのかというとそんな気はしません。
ただの中間生成物なんでしょうか。

テストも通りましたので、コミットします。

```
$ git add .
$ git commit
[newtir f808cc5] two-in-a-row?を改善
 1 file changed, 4 insertions(+), 4 deletions(-)
$ git log --oneline --decorate --graph --all
* f808cc5 (HEAD -> newtir) two-in-a-row?を改善
* f763eb5 (master) 答え合わせ版
* 9eeaf5e two-in-a-row(自分で書いた版)を追加
* a575d5d (origin/master) 「Scheme修行(1) 始める前に」を追加
* 398b4e2 最初のコミット
```

newtirブランチが一歩進みました。
HEADはnewtirを指したままです。
masterブランチはそのままになっています。

### two-in-a-row? 第3弾

`is-first-b`が`two-in-a-row?`を呼んだ後、`two-in-a-row?`が何をするかといえば、無駄に`lat`が空かどうかをチェックして`is-first-b`を呼んでいるだけです。
ということは、`is-first-b`自身が`is-first-b`を呼べばいいんですね。

two-in-a-row.rkt(抜粋)

```
(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (is-first-b? (car lat) (cdr lat)))))))
```

### チェックアウト・マージ

コミットします。

```
$ git commit -a
[newtir 68cff0f] two-in-a-row?をさらに改善
 1 file changed, 1 insertion(+), 1 deletion(-)
$ git log --oneline --decorate --graph --all
* 68cff0f (HEAD -> newtir) two-in-a-row?をさらに改善
* f808cc5 two-in-a-row?を改善
* f763eb5 (master) 答え合わせ版
* 9eeaf5e two-in-a-row(自分で書いた版)を追加
* a575d5d (origin/master) 「Scheme修行(1) 始める前に」を追加
* 398b4e2 最初のコミット
```

無駄なチェックもしなくなったのでいったんここで満足したことにして、masterにマージしてみます。
その前に、まずはmasterブランチに移動します。念のためワーキングディレクトリにやりかけの作業が残ってないことを確認してから、git checkoutでブランチを移動します。エディタやIDEで関連するファイルを開いてないことも確認しないとですね。忘れそうです。別にいいのかな？

```
$ git status
On branch newtir
nothing to commit, working directory clean
$ git checkout master
```

two-in-a-row.rktが以前の版に戻っています。
HEADの場所を確認します。

```
$ git log --oneline --decorate --graph --all
* 68cff0f (newtir) two-in-a-row?をさらに改善
* f808cc5 two-in-a-row?を改善
* f763eb5 (HEAD -> master) 答え合わせ版
* 9eeaf5e two-in-a-row(自分で書いた版)を追加
* a575d5d (origin/master) 「Scheme修行(1) 始める前に」を追加
* 398b4e2 最初のコミット
```

masterがHEADになっています。
思った通りの状態になっているので、newtirをマージします。

```
$ git merge newtir
Updating f763eb5..68cff0f
Fast-forward
 two-in-a-row.rkt | 8 ++++----
 1 file changed, 4 insertions(+), 4 deletions(-)
```

Fast-forwardというのは、実際にはマージする必要がなくて、ブランチを移動するだけですみました、ってことだそうです。
ブランチの状態を確認します。

```
$ git log --oneline --decorate --graph --all
* 68cff0f (HEAD -> master, newtir) two-in-a-row?をさらに改善
* f808cc5 two-in-a-row?を改善
* f763eb5 答え合わせ版
* 9eeaf5e two-in-a-row(自分で書いた版)を追加
* a575d5d (origin/master) 「Scheme修行(1) 始める前に」を追加
* 398b4e2 最初のコミット
```

### two-in-a-row? 第4弾

しかしここで、`is-first-b?`がもはや`is-first?`よりも`two-in-a-row`に近いことに気がついたので名前を変えることにします。

two-in-a-row.rkt(抜粋)

```
(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (two-in-a-row-b? (car lat) (cdr lat))))))

(define two-in-a-row-b?
  (lambda (preceeding lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) preceeding)
                    (two-in-a-row-b? (car lat) (cdr lat)))))))
```

最初の要素だけ特別扱いだよ、っていうことが素直にあらわれた構造でいい感じです。
で、two-in-a-row?で著者は何を伝えたかったんでしょうか。

### 完成、そしてpush

ああっとブランチがmasterのままだ！こういうの忘れそうなんだよな・・・しれっとcheckoutしてみよう。

```
$ git checkout newtir
M	two-in-a-row.rkt
Switched to branch 'newtir'
```
なんか通った。Mとかついてるのは変更されてますよってことかな。変更されてますよ、って止まっちゃうかと思ったけど。許される範囲なのか。

コミット・マージも問題ありません。

```
$ git add .
$ git commit
$ git checkout master
Switched to branch 'master'
$ git merge newtir
Updating 68cff0f..bf4671b
Fast-forward
 two-in-a-row.rkt | 10 +++++-----
 1 file changed, 5 insertions(+), 5 deletions(-)
$ git log --oneline --decorate --graph --all
* bf4671b (HEAD -> master, newtir) two-in-a-row?さらにさらに改善版
* 68cff0f two-in-a-row?をさらに改善
* f808cc5 two-in-a-row?を改善
* f763eb5 答え合わせ版
* 9eeaf5e two-in-a-row(自分で書いた版)を追加
* a575d5d (origin/master) 「Scheme修行(1) 始める前に」を追加
* 398b4e2 最初のコミット
```

ここで完成ということにして、pushします。
-uをつけるとリモートのリポジトリを覚えててくれて、git pushだけでいけるそうです。

```
$ git push -u origin master
Counting objects: 15, done.
Delta compression using up to 4 threads.
Compressing objects: 100% (15/15), done.
Writing objects: 100% (15/15), 2.18 KiB | 0 bytes/s, done.
Total 15 (delta 4), reused 0 (delta 0)
To https://github.com/koba925/seasonedschemer.git
   a575d5d..bf4671b  master -> master
Branch master set up to track remote branch master from origin.
latour:seasonedschemer takahiro$ git push
Everything up-to-date
```

なんとなくひと区切りついたところでpushしましたが、どんなタイミングでpushするんでしょうね。
commitもひと区切りですが、pushはさらに大きな塊っぽいですね。
commitはローカルですが、pushは公開することになりますし。* git

だいたいの流れとしては、branch → checkout → 修正 → diff → add → commit → log → checkout master → merge → 繰り返す → push くらいの感じでしょうか。

## まとめ？
今回は関数ひとつやっただけでgit練習のほうがメインっぽい感じになってしまいましたが、もうちょっとしたら使う分はひととおりマスターできて、Scheme修行のほうに重点をおけると信じています。
コミットしてるおかげで古いソースを安心して上書きしていくことができますが、お勉強用としては古いソースもコメントアウトするとかして残ってたほうがいい気もします。
コメントアウトで残してコミットすればいいんですけどね。

## ここまでで使ったgitコマンド

ちょっと復習。

* git init リポジトリの作成
* git remote リモートリポジトリ名を表示
  * git remote add リモートリポジトリ名を追加
  * git remote show リモートリポジトリの状態を表示
* git status 現在の状態を表示
* git diff 差分を表示
* git add 修正したファイルをステージ
* git commit ステージしたファイルをコミット
  * git commit -a 修正したファイルをステージしてコミット
* git log コミットログを表示
  * git log --oneline 1行形式で表示
  * git log --decorate ブランチも表示
  * git log --graph グラフを表示
  * git log --all 全部表示 何が全部なのかよくわからない
* git branch ブランチを作成
* git checkout ブランチに移動
* git merge ブランチをマージ
* git push リモートリポジトリに送信
  * git push -u デフォルトのリモートリポジトリとして記憶
* git reset なかったことにする
  * git reset HEAD 最新のコミットをなかったことにする