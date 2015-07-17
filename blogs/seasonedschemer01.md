# Scheme修行(1) 始める前に
Scheme手習いのころからやってみようと思ってたことを。

## Racketで書こう
DrRacketはRacketという言語用の環境なので、純粋にSchemeで書こうとするとR5RS使いますよと宣言するのが面倒だったりします。
これからはRacketで書くことにします。
RacketはSchemeの拡張なので、そのまま書いて問題ないはず。

Racketで書く場合にはファイルの先頭に言語の宣言を入れます。

```
#lang racket
```

または

```
#lang racket/base
```

racket/baseは最低限の仕様、racketはいろんなライブラリをインポート済みとなっています。
ここではracket/baseで十分と思われます。
宣言だけすればあとは普通のSchemeと同じに書けます。普通のカッコの代わりに角カッコを使うとかもできますが、当面気にしないことにします。

ちなみに`#lang algol60/algol60` と宣言するとAlgol 60で書けるという謎仕様です。
Algol 60用の処理系を別に持っているわけではなく、Racketのマクロ機能で実現しているということで拡張性の高さに驚かされます。他のLispでもできるのかもしれませんが、わざわざ標準で入れているというのはきっと自慢なんでしょう。
その他、オブジェクトにパターンマッチに契約にといろいろな機能が用意されています。
Racketそのものを型付きにしたりデフォルト遅延評価にした拡張もあります。
大学の先生がやる気出していろいろ追加してるようです。

## モジュールを使おう
scheme手習いでは、`atom?`や`add1`、`first`、`second`、`build`など、ユーティリティ的に使われる関数がいくつかありました。
こういった関数を別ファイルに入れられるよう、Racketのモジュール機能を使ってみます。
こんな形です。

util.rkt

```
#lang racket/base

; エクスポートする関数の宣言
(provide atom? add1 sub1)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n) (+ n 1)))

(define sub1
  (lambda (n) (- n 1)))
```

モジュールを使う側のソース

```
#lang racket/base

(require "util.rkt")
```

本体側も適当な単位に分割するようにします。

## テストを書こう

ついでに、ユニットテストも軽く体験しておこうと思います。
RackUnitというユニットテスト用のモジュールが標準装備されているので、これを使います。
util.rktに、もっとも単純な形でテストを追加しました。

util.rkt

```
#lang racket/base

(require rackunit)

(provide atom? add1 sub1)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(check-true (atom? 0))
(check-true (atom? (quote a)))
(check-false (atom? (quote ())))
(check-false (atom? (quote (a b))))

(define add1
  (lambda (n) (+ n 1)))

(check-equal? (add1 1) 2)

(define sub1
  (lambda (n) (- n 1)))

(check-equal? (sub1 2) 1)
```

本体とテストのファイルを分割することもできますがとりあえずは同居させてます。必要を感じたら分けようと思います。
成功すると何もでてこないので寂しいですね。というか本当に実行してるのか心配です。
失敗するとこんな表示になります。

ソース

```
(define add1
  (lambda (n) (+ n 2)))

(check-equal? (add1 1) 2)
```

実行結果

```
--------------------
FAILURE
actual:     3
expected:   2
name:       check-equal?
location:   (#<path:/Users/takahiro/Dropbox/study/seasonedschemer/util.rkt> 19 0 302 25)
expression: (check-equal? (add1 1) 2)

. . Check failure
--------------------
```

scheme修行では代入が出てはきますが、入出力がないのでテストはそんなに複雑にはならないんじゃないかと思います。逆に、体験したことになるのかどうかが心配なくらい。

## バージョン管理してみよう

恥ずかしながらまともにバージョン管理をしたことがないので、これもやってみます。
リモートにはGitHubを使おうかと思います。一人プロジェクトなんであんまり意味無いですけど。
[Pro Git](https://progit.org) の1、2章を読みつつ進めます。
Pro Gitがコマンドライン中心なのと、コマンドラインがわかってたほうが気分がいい気がするのでコマンドラインでやってみる方針で。

1. gitをインストールします。デフォルトで入るgitは少々古いので、念のため [Git - Downloads](http://git-scm.com/downloads) から最新のgitをダウンロードしてインストールします。README.txtを読むこと。
gitの初期設定を行います。

	```
	$ git config --global user.name "Takahiro Kobayashi"
	$ git config --global user.email "koba925@gmail.com"
	```

1. GitHubのパスワードをキャッシュするようにします。

	```
	$ git config --global credential.helper osxkeychain
	```

	参考：[Caching your GitHub password in Git](https://help.github.com/articles/caching-your-github-password-in-git/)

1. 作業フォルダに移動してリポジトリを作ります。

	```
	$ git init
	```

1. お勉強のため、こまめに状態を確認しながら進めます。

	```
	$ git status
	```

1. .gitignoreファイルで無視するファイル・ディレクトリを指定します。

	```
	$ vi .gitignore
	compiled/
	*~
	```

1. .gitignore、util.rktをステージングします。

	```
	$ git add .gitignore
	$ git add util.rkt
	```

	うまく.gitignoreが働いてくれれば、git add *でOKな模様。

1. 最初のコミット。エディタの設定はしていないのでvimでコメントを書きます。

	```
	$ git commit
	```

1. コミットのログを確認します。

	```
	$ git log
	```

1. リモートのリポジトリにプッシュするため、https://github.org にアクセスしてアカウントを作ります。

1. ログインしたら新しいリポジトリを作成します。名前を入力するだけでほかはとりあえず何もいじっていません。名前はseasonedschemerにしました。

1. リモートのリポジトリに名前をつけます。最初はoriginという名前にするのが基本のようです。

	```
	$ git remote add origin https://github.com/koba925/seasonedschemer.git
	$ git remote
	```
	
1. ローカルからリモートへアップロードします。

	```
	$ git push origin master
	```

1. リモートの状態を確認しておきます。

	```
	$ git remote show origin
	```

1. ブラウザで確認します。

とりあえずこんなところで。
あとはaddとcommitとpushを繰り返していればとりあえず最低限のバージョン管理になりそうです。
その他の機能は使うチャンスを見計らいつつ。

今はDropBoxにファイルを保存しているんですが、その上でGitを使うというところがそこはかとなく不安です。
あっちとこっちでcommitするとか。
まあひとりでしか使わないのでおかしなことにはならないと信じてます。

## 記事を書いたところで

ファイルを保存して、HTMLにエクスポートして、git addしよう。

```
$ git status
On branch master
Untracked files:
  (use "git add <file>..." to include in what will be committed)

	blogs/

nothing added to commit but untracked files present (use "git add" to track)
```

ディレクトリ名だけ表示されてるな。
ディレクトリで指定してもいいのかな？

```
$ git add blogs
```

git addしても何も言わないんだな。うまくいった？

```
$ git status
On branch master
Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

	new file:   blogs/seasonedschemer01
	new file:   blogs/seasonedschemer01.html
```

うまく・・・いってない！Markdownの拡張子忘れてる。取り消し。

```
$ git reset HEAD blogs/seasonedschemer01
$ git status
On branch master
Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

	new file:   blogs/seasonedschemer01.html

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	blogs/seasonedschemer01
```

ファイル名を変更して再度。

```
$ mv blogs/seasonedschemer01 blogs/seasonedschemer01.md
$ git status
On branch master
Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

	new file:   blogs/seasonedschemer01.html

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	blogs/seasonedschemer01.md

$ git add blogs
$ git status
On branch master
Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

	new file:   blogs/seasonedschemer01.html
	new file:   blogs/seasonedschemer01.md
```

こんどはうまくいったのでコミットしてプッシュ。
さっそくひとつ技を覚えました！