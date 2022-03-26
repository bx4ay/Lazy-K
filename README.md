# Lazy-K
[Lazy K](http://tromp.github.io/cl/lazy-k.html) のインタプリタ

`README.md` - これ  
`lazy.hs` - Lazy K のインタプリタ

## Lazy K ってなに
コンビネータ計算を記述する言語です。

以下の 4 つの記法を組み合わせて記述されます。
- コンビネータ計算様式 - `S` ( *λ x y z*. *x z* (*y z*) )、`K` ( *λ x y*. *x* )、`I` ( *λ x*. *x* )、`(`、`)`
- Unlambda 様式 - `s` ( *λ x y z*. *x z* (*y z*) )、`k` ( *λ x y*. *x* )、`i` ( *λ x*. *x* )、`` ` `` (関数適用)
- Iota 様式 - `i` ( *λ x*. *x* S K )、`*` (関数適用)
- Jot 様式 - ` `&nbsp;( I )、\[ *x* \]`0` ( *x* S K )、\[ *x* \]`1` ( S (K *x*) )

プログラムは入力を受け取り、出力を返す関数として扱われます。

文字は Church 数、文字列はそれらの Church リストとしてエンコードされます。EOF は 256 です。

## インタプリタについて
例：
```console
$ ghc lazy.hs
$ cat drop7.lazy
``s`k``si`k`sk``s`k``si`k`sk``s`k``si`k`sk``s`k``si`k`sk``s`k``si`k`sk``s`k``si`k`sk``si`k`sk
$ cat test.in
Hello, world!
$ ./lazy drop7.lazy < test.in
world!
```
(`drop7.lazy`は入力の 8 文字目以降を返すプログラム)
- 入力は標準入力から読み込まれ、出力は標準出力に書き込まれます。
- ソースコードに含まれる``()*01IKS`iks``以外の文字は無視されます。
- `[program-file].lazy`の代わりに`-e "[program-code]"`と記述すれば、`[program-code]`自体が Lazy K のコードとして解釈・実行されます。
- 複数のプログラムを引数として与えた場合、それらは逆向きに関数合成されます。つまり、`lazy code1 code2`は`lazy code1 | lazy code2`のような意味になります。

### できないこと
- η-変換
- 適切なエラーを出力すること
