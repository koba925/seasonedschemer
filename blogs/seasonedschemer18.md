# Scheme修行 (18) ループしたリスト
最後のkonsを取り出すlast-konsです

```
(define last-kons
  (lambda (ls)
    (cond ((null? (kdr ls)) ls)
          (else (last-kons (kdr ls))))))
```

なんのへんてつもありません

```
> (define long (lots 12))
> (wride long)
(egg egg egg egg egg egg egg egg egg egg egg egg)

> (wride (last-kons long))
(egg)

> (lenkth long)
12
```

longをいじってやります

```
> (set-kdr (last-kons long) long)
> (lenkth long)
このプログラムはメモリを使いきりました。
Interactions disabled
> (wride egg)
(egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg
egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg
egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg
egg egg egg egg egg ...
```

longは最初こうでした

```
long
 ↓
egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->() 
```

(set-kdr (last-kons long) long)を実行した後はこうなってます

```
long
 ↓
egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg
 ↑                                                      |
 +------------------------------------------------------+
```

ので、kdrを順番にたどっていくといつまでたっても終わりになりません
こうしても同じ

```
> (define long (lots 12))
> (set-kdr (last-kons long) (kdr (kdr long)))
> (lenkth long)
このプログラムはメモリを使いきりました。
Interactions disabled
> (wride egg)
(egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg
egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg
egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg egg
egg egg egg egg egg ...
```

こんどはこういう絵になってます

```
long
 ↓
egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg->egg
           ↑                                            |
           +--------------------------------------------+
```

絵で書いてごまかしましたが本当はクロージャです
んーとどう書くといいかな・・・

```
(仮引数→(selector) 関数本体→(selector kar kdr) kar→egg kdr→
  (仮引数→(selector) 関数本体→(selector kar kdr) kar→egg kdr→
あ→ (仮引数→(selector) 関数本体→(selector kar kdr) kar→egg kdr→
      (仮引数→(selector) 関数本体→(selector kar kdr) kar→egg kdr→
         ...
                (仮引数→(selector) 関数本体→(selector kar kdr) kar→egg kdr→あ))...)
```

書けばよく分かるってものでもないかな

そんなリストだったら#fを返し、普通のリストだったら長さを返す関数finite-lenkthです

```
(define finite-lenkth
  (lambda (p)
    (let/cc infinite
      (letrec ((C (lambda (p q)
                    (cond ((same? p q) (infinite #f))
                          ((null? q) 0)
                          ((null? (kdr q)) 1)
                          (else (+ (C (sl p) (qk q)) 2)))))
               (qk (lambda (x) (kdr (kdr x))))
               (sl (lambda (x) (kdr x))))
        (cond ((null? p) 0)
              (else (add1 (C p (kdr p)))))))))
```

なんでしょうこれは？
本体はCで、Cを呼ぶたびにpはリスト内を１つ進み、qは2つ進む？
でpとqが同じだったら終わる？
ループができてる場合はどう動くのかな？

よくわからないので動かしてみます
まずは普通のリストから

```
   1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> ()
0  p    q                                +1
1       p         q                      +2
2            p              q            +2
3                 p                   q  +2
```

ここで(null? q)なのでCが0を返し、0+2+2+2+1で7になる、と

ああ、qの方だけ見るとちょっと変わったlenkth関数に見えますね

```
(define finite-lenkth1
  (lambda (p)
    (letrec ((C (lambda (q)
                  (cond ((null? q) 0)
                        ((null? (kdr q)) 1)
                        (else (+ (C (qk q)) 2)))))
             (qk (lambda (x) (kdr (kdr x)))))
      (cond ((null? p) 0)
            (else (add1 (C (kdr p))))))))
```

さらに単純化すると

```
(define lenkth2
  (lambda (q)
    (cond ((null? q) 0)
          ((null? (kdr q)) 1)
          (else (+ (lenkth2 (kdr (kdr q))) 2)))))
```

普通のlenkthと見比べます

```
(define lenkth
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (lenkth (kdr l)))))))
```

lenkth2のほうは２つずつ進めているので、

* 2ずつ足している
* qがnull?かどうかだけでなく、(kdr q)がnull?かどうかもチェックしている

という違いがあるだけですね

では、ループができている場合について確認します

```
        +------------------------+
        ↓                        |
   1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7
0  p    q
1       p         q
2            p              q
3       q         p
4                 q    p
5                          p,q
```

で(same? p q)となって#fを返すというわけかー
qはとびとびに進んでいるのですれ違ったりしないかと思いましたが
pがひとつずつ、qがふたつずつ進んでいるので差がひとつずつ縮まっていくだけだから
心配いらなかったですね

うまいこと考えるもんだなあ
てっきりO(n^2)かと思ったけどO(n)で済むのかー
