# Scheme修行(4) 第12章 multirember & 第12の戒律

## gitのタグ付け

11章が終わったところでタグ付けもやってみます。

```
$ git tag -a chap11
$ git tag
chap11
$ git show chap11
tag chap11
Tagger: Takahiro Kobayashi <koba925@gmail.com>
Date:   Sun Jul 26 10:05:19 2015 +0900

第11章完了

commit 6776a044d33221e43fe235d90cef0f6f69c3a05b
Author: Takahiro Kobayashi <koba925@gmail.com>
Date:   Thu Jul 23 07:56:58 2015 +0900

    「Scheme修行(3) gitと少し1章の残り」を追加

diff --git a/blogs/seasonedschemer03.html b/blogs/seasonedschemer03.html
new file mode 100644
index 0000000..e649dc2
--- /dev/null
+++ b/blogs/seasonedschemer03.html
@@ -0,0 +1,565 @@
+<!DOCTYPE html><html>
:
:
```

タグは明示的にpushしないとサーバに上がらないそうです。

```
$ git push origin chap11
Counting objects: 1, done.
Writing objects: 100% (1/1), 184 bytes | 0 bytes/s, done.
Total 1 (delta 0), reused 0 (delta 0)
To https://github.com/koba925/seasonedschemer.git
 * [new tag]         chap11 -> chap11

```

githubのWebだとタグって出てこないのかな・・・出てきたところであんまり使い道も思いつきませんが。
そもそもタグって具体的にはどうつかうんですかね。タグ名でチェックアウトすることはできないようですし。わかるようなわからないような。

## gitのエイリアス

コマンドを入力しやすいよう、エイリアスを定義することができるらしいのでやってみます。
デフォルトでけっこう定義されていました。

```
$ git alias
a	 => !git add . && git status
aa	 => !git add . && git add -u . && git status
ac	 => !git add . && git commit
acm	 => !git add . && git commit -m
alias	 => !git config --list | grep 'alias\.' | sed 's/alias\.\([^=]*\)=\(.*\)/\1\	 => \2/' | sort
au	 => !git add -u . && git status
c	 => commit
ca	 => commit --amend
cm	 => commit -m
d	 => diff
l	 => log --graph --all --pretty=format:'%C(yellow)%h%C(cyan)%d%Creset %s %C(white)- %an, %ar%Creset'
lg	 => log --color --graph --pretty=format:'%C(bold white)%h%Creset -%C(bold green)%d%Creset %s %C(bold green)(%cr)%Creset %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative
ll	 => log --stat --abbrev-commit
llg	 => log --color --graph --pretty=format:'%C(bold white)%H %d%Creset%n%s%n%+b%C(bold blue)%an <%ae>%Creset %C(bold green)%cr (%ci)' --abbrev-commit
master	 => checkout master
s	 => status
spull	 => svn rebase
spush	 => svn dcommit
```

a(add)、c(commit)、ac(add+commit)、d(diff)、l(log)、s(status)がすでに定義されていますので、繰り返し使うものはかなりカバーできてる感じです。
branchとかcheckoutはそれに比べると使用頻度が低いということでしょうか。

練習がてら定義してみます。

```
$ git config --global alias.br branch
$ git config --global alias.co checkout
$ git alias
:
br	 => branch
:
co	 => checkout
:
```

あとは使うのが大事。使わないと覚えないし忘れるし。
気がついたら定義していきます。

## multirember

第12章「避難しましょう」ではまず`multirember`を題材にします。

```
#lang racket/base

(require rackunit)
(require "util.rkt")

(define multirember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))

(check-equal? (multirember 'a '()) '())
(check-equal? (multirember 'a '(a b)) '(b))
(check-equal? (multirember 'a '(b c)) '(b c))
(check-equal? (multirember 'a '(a b a)) '(b))
```

> `multirember`が`lat`を横断すると、`a`は変わりますか。
> 
> いいえ、`a`はいつも`tuna`を表しています。
> 
> それなら、`a`がずっと`tuna`を表していることを、自然な再帰のたびに`multirember`に思い出させる必要はないですよね。
> 
> はい。このような関数を読むときは、そのほうがとても助かります。

一理あると思いますが、助かるっていうのは人間が助かる話なんでしょうか。それともCPUが助かる？

Yを使って書くとこのようになります。

```
(define multirember
  (lambda (a lat)
    ((Y
      (lambda (mr)
        (lambda (lat)
          (cond ((null? lat) (quote ()))
                ((eq? (car lat) a) (mr (cdr lat)))
                (else (cons (car lat) (mr (cdr lat))))))))
     lat)))
```

これを以下のように書くことができます。

```
(define multirember-r
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond ((null? lat) (quote ()))
                      ((eq? (car lat) a) (mr (cdr lat)))
                      (else (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))
```

`(letrec ((mr ...)) mr)`はmrという名前の再帰関数を定義して、その再帰関数を返しています。なんとなく冗長なことをやってる響きです。
手習いの`value`に`define`をつけようとして苦労した経験もあり、`mr`の定義の中で`mr`自身を呼べるというのが`letrec`の大事なところっぽいということがわかります。
ていうか、手習いのときに付けたのはdefineよりもletrecに近い感じです。

Yと同等のことがわかりやすく書けます、といったところ。

> でも、欲しいのが再帰関数`mr`なら、なぜこうしないのですか。
> 
> ```
> (define mr
>   (lambda (lat)
>     (cond ((null? lat) (quote ()))
>           ((eq? a (car lat)) (mr (cdr lat)))
>           (else (cons (car lat) (mr (cdr lat)))))))
> 
> (define multirember-d
>   (lambda (a lat) (mr lat)))
> ```

まあ普通にダメって感覚なんですが、Schemeは静的スコープの言語だから、ってことでしょう。
聞きかじりの知識からすると、動的スコープな言語ならこれでも動くんでしょうか。

これならSchemeでもちゃんと動きます。

```
(define multirember-d
  (lambda (a lat)
    (define mr
      (lambda (lat)
        (cond ((null? lat) (quote ()))
              ((eq? a (car lat)) (mr (cdr lat)))
              (else (cons (car lat) (mr (cdr lat)))))))
    (mr lat)))
```

ほとんど同じなんですが、こちらではなくて`letrec`を使っているのは、コンピュータサイエンス的に`letrec`のほうが由緒正しい書き方だってことなんでしょうか？

こういう書き方もできます。

```
(define multirember-r2
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond ((null? lat) (quote ()))
                     ((eq? (car lat) a) (mr (cdr lat)))
                     (else (cons (car lat) (mr (cdr lat))))))))
      (mr lat))))
```

ますます`define`を使った版とそっくりですね。

> Yよりはましです。

はげしく同意です。

## 第12の戒律

> 再帰を適用している間に変化せぬ引数を除くには`letrec`を用いるべし。

確かに`a`を渡さなくて済むようになりましたが、カッコが増えました。
ムダが減っていることは確かなんですが、読みやすくなったんでしょうか？
戒律は、「変化せぬ引数を除け」とは言ってないので、読みやすくなるなら除けばいい？
わかりません。

## さらにmultirember

同一性判定の関数を引数に取る`multirember`を作ります。

```
(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) (quote ()))
            ((test? (car l) a) ((multirember-f test?) a (cdr l)))
            (else (cons (car l) ((multirember-f test?) a (cdr l))))))))
```

`(multirember-f test?)`の値は繰り返しの間変化しないので、`letrec`を使います。

```
(define multirember-f2
  (lambda (test?)
    (letrec
        ((m-f
          (lambda (a l)
            (cond ((null? l) (quote ()))
                  ((test? (car l) a) (m-f a (cdr l)))
                  (else (cons (car l) (m-f a (cdr l))))))))
      m-f)))
```

`multirember-f2`に`eq?`を渡してやります。

```
(define multirember-r3
  (letrec
      ((mr (lambda (a lat)
             (cond ((null? lat) (quote ()))
                   ((eq? (car lat) a) (mr a (cdr lat)))
                   (else (cons (car lat) (mr a (cdr lat))))))))
    mr))
```

さらに、`mr`を`multirember`に変えます。何をやっているんでしょうか。

```
(define multirember-r4
  (letrec
      ((multirember
        (lambda (a lat)
          (cond ((null? lat) (quote ()))
                ((eq? (car lat) a) (multirember a (cdr lat)))
                (else (cons (car lat) (multirember a (cdr lat))))))))
    multirember))
```

この`letrec`は定義した関数をそのまま返しているだけですので取り除くことができます。

```
(define multirember-r5
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) a) (multirember-r5 a (cdr lat)))
          (else (cons (car lat) (multirember-r5 a (cdr lat)))))))
```

> おなじみの`multirember`に戻るでしょう。

どうやら、`multirember-f`に`eq?`を渡すと`multirember`になることを論証？してたようです。

## 今回覚えたgitコマンド

やっと落ち着いてきました。

* git tag タグを表示
* git tag -a アノテーテッドタグを付ける（こっちが普通）
* git show オブジェクトを表示する
* git alias エイリアスを表示する