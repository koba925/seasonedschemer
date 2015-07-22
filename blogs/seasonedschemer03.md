# Scheme修行(3) gitと少し1章の残り

## 作業用ブランチの後始末

newtirブランチが残ったままでした。もう使わないので削除します。
ブランチの用途によっては恒久的に残すものもありそうです。

```
$ git log --oneline --decorate --graph --all -3
* ea4da73 (HEAD -> master, origin/master) 「Scheme修行(2) はじめに ＆ two-in-a-row?」を追加
* bf4671b (newtir) two-in-a-row?さらにさらに改善版
* 68cff0f two-in-a-row?をさらに改善
$ git branch -d newtir
Deleted branch newtir (was bf4671b).
$ git log --oneline --decorate --graph --all -3
* ea4da73 (HEAD -> master, origin/master) 「Scheme修行(2) はじめに ＆ two-in-a-row?」を追加
* bf4671b two-in-a-row?さらにさらに改善版
* 68cff0f two-in-a-row?をさらに改善
```

消えました。git branchでブランチ名を入れなければブランチのリストが表示されます。

```
$ git branch
* master
```

ブランチを消してから気が付きましたが、消す前に一度やっておいたほうがよさそうです。

やっぱり1関数1ファイルは細かすぎる気がするので、章ごとにまとめるくらいにしようと思います。git mvを使うとgitの中の情報とファイル名が同時に変更されます。

```
$ ls
blogs			two-in-a-row.rkt
compiled		util.rkt
$ git mv two-in-a-row.rkt chap1.rkt
$ ls
blogs		chap1.rkt	compiled	util.rkt
$ git status
On branch master
Your branch is up-to-date with 'origin/master'.
Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

	renamed:    two-in-a-row.rkt -> chap1.rkt

$ git commit -m 'ファイル名変更'
[master 72934fa] ファイル名変更
 1 file changed, 0 insertions(+), 0 deletions(-)
 rename two-in-a-row.rkt => chap1.rkt (100%)

```

別にgit mvでファイル名を変更しないと管理情報とのずれが発生するとかそういう怖い話はなくて、mvでもその他のツールでも使ってファイル名を変更した後、git add・git rmしてやってもいいようです。
ファイル名の変更も割合気楽にできそうです。

## sum-of-prefixes

タップを引数に取り、タップの各要素までの和をリストにして返す関数sum-of-prefixesを作ります。
今度はチラ見後に書いたコードでテキストと一致しました。

chap1.rkt(抜粋)

```
(define sum-of-prefixes
  (lambda (tup)
    (cond ((null? tup) (quote ()))
          (else (sum-of-prefixes-b 0 tup)))))

;+はあることにする
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond ((null? tup) (quote ()))
          (else (cons (+ sonssf (car tup))
                      (sum-of-prefixes-b (+ sonssf (car tup))
                                         (cdr tup)))))))
```

どうやらこの章のテーマは過去の情報を引数で渡すということのようです。
それは大事な概念なんでしょうか？
何かの前振り？

# コミットメッセージの書き換え

直前のコミットのコミットメッセージはgit commit --amendで修正することができます。

```
$ git log --oneline -3
32cd3bc sum-of-prefixes(自分で書いた版)を追加
72934fa ファイル名変更
ea4da73 「Scheme修行(2) はじめに ＆ two-in-a-row?」を追加
$ git commit --amend -m 'sum-of-prefixesを追加'
[sop 63e9da4] sum-of-prefixesを追加
 Date: Sat Jul 18 11:14:08 2015 +0900
 1 file changed, 16 insertions(+)
$ git log --oneline -3
63e9da4 sum-of-prefixesを追加
72934fa ファイル名変更
ea4da73 「Scheme修行(2) はじめに ＆ two-in-a-row?」を追加
```

--amendはコミットメッセージを書き換えるオプションというわけではなくて、直前のコミットをやり直す機能なので、addし忘れたとかのときも使えるようです。

## 第11の戒律

> ある関数が、その関数に対する他の引数がいかなるものか知る必要があるときは、付加的な引数を用いるべし。

おお、なるほど！という感じはあまりしません。
理解が足りないんでしょうか。
5回読んだらおおなるほどってなる？
訳がこなれてない感じもします。
名訳だったらなるほどかっていうとそんな気もしませんが。

## scrambleって何

次はscarambleという関数を作ります。
自分では説明できないので引用します。

> 関数scrambleは、どの数も自分の位置を示す番号より大きくない空でないタップを取って、同じ長さのタップを返します。
> 引数の中のそれぞれの数は、自分を起点としてタップを逆方向にさかのぼる数として扱われます。
> 各位置の結果は、その数のぶんだけ現在の位置から逆向きにたどることで見つけられます。

これはコード見たほうがわかりやすいかも・・・

## stash

scrambleを定義するのに、one?とpickが必要です。
これは、util.rktに定義します。
util.rktの修正用にブランチを切りたいと思います。
しかし、scrambleを途中まで書いてしまいました。
半端な状態なのでコミットはしたくありません。
このままブランチを切ってチェックアウトすると、util.rktの修正の方にscrambleの修正が混じってしまいそうです。
addしなきゃうまくいくのかもしれませんが、そのことに気づいたのは全部終わってからでした。

困りました。なんとかならないでしょうか。
別ディレクトリにクローンして修正するとか？それもないではなさそうですが・・・
調べてみると、git stashというのがそれ用のコマンドのようです。
詳しくは[7.3 Git Tools - Stashing and Cleaning]
(http://git-scm.com/book/en/v2/Git-Tools-Stashing-and-Cleaning)を参照。

git stashを実行すると、現在の状態がスタックに保存され、ワーキングディレクトリはコミット直後のクリーンな状態に戻されます。
他の作業をしてから元のブランチに戻り、git stash applyすると、ワーキングディレクトリが元の状態に戻ります。

やってみます。

```
$ git status
On branch scramble
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

	modified:   chap1.rkt

$ git stash
Saved working directory and index state WIP on scramble: 63e9da4 sum-of-prefixesを追加
HEAD is now at 63e9da4 sum-of-prefixesを追加
$ git status
On branch scramble
nothing to commit, working directory clean
```

ワーキングディレクトリが元に戻りましたので、いったんmasterブランチに戻り、util.rkt修正用のブランチを切って、修正を行います。
git log に --all オプションをつけるとスタッシュも表示されますね。やっぱり --all つけといたほうがいいですね。--graphオプションもやっとグラフっぽいものを表示してくれました。

```
$ git log --oneline --decorate --graph --all -5
* 50e77c8 (HEAD -> util) one?とpickを追加
| *   8d96ec1 (refs/stash) WIP on scramble: 63e9da4 sum-of-prefixesを追加
| |\  
|/ /  
| * 7d5e608 index on scramble: 63e9da4 sum-of-prefixesを追加
|/  
* 63e9da4 (scramble, master) sum-of-prefixesを追加
* 72934fa ファイル名変更
```

utilブランチの修正をmasterにマージしておきます。

git stash apply で保存したスタッシュをワーキングディレクトリに適用します。スタッシュは削除されません。スタッシュを適用してもブランチは現在のブランチのままのようですので、scrambleをチェックアウトしてからスタッシュを適用します。

```
$ git checkout scramble
Switched to branch 'scramble'
$ git stash apply
On branch scramble
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

	modified:   chap1.rkt

no changes added to commit (use "git add" and/or "git commit -a")
$ git log --oneline --decorate --graph --all -5
* 50e77c8 (util, master) one?とpickを追加
| *   8d96ec1 (refs/stash) WIP on scramble: 63e9da4 sum-of-prefixesを追加
| |\  
|/ /  
| * 7d5e608 index on scramble: 63e9da4 sum-of-prefixesを追加
|/  
* 63e9da4 (HEAD -> scramble) sum-of-prefixesを追加
* 72934fa ファイル名変更
```

無事scrambleに戻れましたのでgit stash dropで スタッシュを削除します。コミットログもすっきりしました。

```
$ git stash drop
Dropped refs/stash@{0} (8d96ec1822809717f948d81c0ed2b55ef2e1702a)
$ git log --oneline --decorate --graph --all -5
* 50e77c8 (util, master) one?とpickを追加
* 63e9da4 (HEAD -> scramble) sum-of-prefixesを追加
* 72934fa ファイル名変更
* ea4da73 (origin/master) 「Scheme修行(2) はじめに ＆ two-in-a-row?」を追加
* bf4671b two-in-a-row?さらにさらに改善版
```

あとはmasterブランチの内容をマージして作業を続行します。

スタッシュ関連ではほかにもいくつかコマンドがあります。git stash list はスタッシュの一覧を、git stash show はスタッシュの内容を表示します。

```
$ git stash list
stash@{0}: WIP on scramble: 63e9da4 sum-of-prefixesを追加
$ git stash show
 chap1.rkt | 27 +++++++++++++++++++++++++++
 1 file changed, 27 insertions(+)
```

git stash pop は保存したスタッシュをワーキングディレクトリに適用し、スタッシュを削除します。慣れたらこっちのほうが速いかもしれませんが、不安を感じるうちは思ったとおりに復元できてからdropしたほうがよさそうです。

## scrambleの定義

scrambleの定義は以下のとおりです。

chap1.rkt(抜粋)

```
(define scramble
  (lambda (tup)
    (scramble-b tup (quote ()))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond ((null? tup) (quote ()))
          (else (cons (pick (car tup) (cons (car tup) rev-pre))
                      (scramble-b (cdr tup)
                                  (cons (car tup) rev-pre)))))))
```

戒律通りですね、という以外にあまり書くことがありません・・・

## 今回覚えたgitコマンド

やっぱりgitの練習みたいになってしまいました。
始める前はaddとcommitだけでいいかと思っていたんですがついブランチに手を出したら沼にハマり気味です。
今回新しく使ったgitコマンドは以下のとおりです。
記事に書かなかったものも含め記載しています。

* git branch -d ブランチを削除
* git checkout -b ブランチを作成してチェックアウト
* git mv ファイル名を変更
* git log -<n> 新しい方からn個分のコミットだけ表示
* git commit -m コマンドラインでコミットメッセージを指定
* git commit --amend 直前のコミットをやり直す
* git stash スタッシュに保存
* git stash list 保存されたスタッシュの一覧
* git stash show スタッシュの内容を表示
* git stash apply 保存されたスタッシュを適用
* git stash drop 保存されたスタッシュを削除
* git stash pop 保存されたスタッシュを適用して削除