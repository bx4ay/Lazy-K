# Lazy-K
[Lazy K](http://tromp.github.io/cl/lazy-k.html) のインタプリタ

`README.md` - これ  
`lazy.hs` - Lazy K のインタプリタ

こんな感じで動きます：
```console
$ ghc lazy.hs
$ cat reverse.lazy
`````isii``s`k`s``s`ks``s`kk``s`k````sii```sii``s``s`kski``s`k`sikk``s`k`s`k`s``s``si`kk`k``s`k`sik``s`k`s`k`s`kk``s``s`ks``s`k`s`ks``s``s`ks``s`kk``s`ks``s`kk``sii`k``s`k`s``s`ks``s`k`si```ss`si`kk``s`kkk`k`k``si`k`ki`k```sii```sii``s``s`kski
$ cat test.in
Hello, world!
$ ./lazy -e "11111110001101110011110010" reverse.lazy < test.in

!dlrow ,olle
```
