# Scheme修行(13) deep最終版

しばらくScratchやってる間に手を出してた何冊かの本が少し落ち着いてきました  
ということでScheme修行再開します  

いろいろ忘れてる気がします  
gitとか  
まず何をするんだっけ？checkoutだっけ？  
そんな調子

scheme自体はそんなに忘れてない気がします  
まあ覚えておかなければいけないことはちょっとしかありませんし  
perlは1ヶ月使わなかったらもう完全に忘れる自信があります  
戒律と掟はどうかなあ  

## 見つからなくても大丈夫なfind

見つからなかったら#fを返すようにfindを書き換えます  

```
(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond ((null? ns) #f)
                    ((= (car ns) n) (car rs))
                    (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))
```

もともとリストしか返さなかった関数なのでこれでOK  
なんでも返す可能性があるんだったら、#fが見つかったのか何も見つからなかったのか  
判別できないのでこのやりかたは使えません  
このあたりがちょっと気持ち悪い  
前も書きましたが  

## deep最終版

このfindを使うと、あらかじめmemberで値が含まれているのを確認してから  
findを呼ぶ、という無駄を省くことができます  
memberもfindもやってるのは似たようなことなのでぜひ消したいところ  

```
(define deep
  (lambda (m)
    (cond ((zero? m) (quote pizza))
          (else (cons (deepM (sub1 m)) (quote ()))))))

(define deepM
  (let ((Ns (quote ()))
        (Rs (quote ())))
    (lambda (n)
      (let ((found (find n Ns Rs)))
        (if found
            found
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result))))))
```

しかし久しぶりに見るせいかNsとRsが役に立っていないような気がしてきました  
ちゃんと前回までの結果覚えててくれてる？  

この前はなんで納得してたんだっけ  
あそうかletがlambdaの外にあるからか  
どれどれデバッガで見てみよう
覚えてる覚えてる  

## おしまい

前回はどうもへんなところで終わってた模様  
ちょっとしか書いてませんがここでキリをつけとかないとおかしいのでここでおしまい  