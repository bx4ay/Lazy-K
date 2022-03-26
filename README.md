# Lazy-K
[Lazy K](http://tromp.github.io/cl/lazy-k.html) のインタプリタ

`README.md` - これ  
`lazy.hs` - Lazy K のインタプリタ

## BLC ってなに
型なしラムダ計算を [de Bruijn インデックス](https://ja.wikipedia.org/wiki/%E3%83%89%E3%83%BB%E3%83%96%E3%83%A9%E3%82%A6%E3%83%B3%E3%83%BB%E3%82%A4%E3%83%B3%E3%83%87%E3%83%83%E3%82%AF%E3%82%B9) 記法で書いてバイナリにエンコードしたもの。

&emsp;\[ *λ* expr \] = `00` \[ expr \]  
&emsp;\[ expr<sub>1</sub> expr<sub>2</sub> \] = `01` \[ expr<sub>1</sub> \] \[ expr<sub>2</sub> \]  
&emsp;\[ *i* \] = `1`<sup>*i*</sup> `0`

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
- 複数のプログラムを引数として与えた場合、それらは逆向きに関数合成されます。つまり、`blc code1 code2`は`blc code1 | blc code2`のような意味になります。
