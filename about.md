## いろいろな確率分布のパラメータをいじくるアプリ

いろいろな確率分布のカタチを見ることができるWebアプリです。  
パラメータをいじくって、確率分布のカタチがどのように変わるのか観察しましょう。  
 **上部メニュー** からお好きな確率分布を選んでください。

このアプリはR言語のWebアプリフレームワークである<a href="http://shiny.rstudio.com/">Shiny</a>
で、<a href="https://twitter.com/ksmzn">@ksmzn</a>が作りました。  
ご指摘や、追加すべき確率分布などがありましたらTwitterで教えてくださると助かります。  
また、全てのコードは<a href='https://github.com/ksmzn/ShinyDistributionsApp'>GitHub</a>にもおいてありますので、拙いコードでよろしければ参考にしてください。

~~時間があれば、グラフをggplot2ではなく、D3.jsなどを使ったインタラクティブなものにしたいですね。~~  
**【2015年1月14日 追記】**  
NVD3.jsを使ったグラフに変更しました！  
また、分布名の隣に、日本版Wikipediaへのリンクを追加しました。

### 参考文献

このアプリを作る際に参考にしたページは以下です。  
特に、まだまだShinyの日本語情報が少ないなか、
<a href="https://twitter.com/hoxo_m">@hoxo_m</a>さんの記事やコードはとても参考になりました。  
<a href="https://twitter.com/hoxo_m">@hoxo_m</a>さん、ありがとうございました。

+ [ボケて(bokete)のネタを全自動で流し見できるサイト作った - ほくそ笑む](http://d.hatena.ne.jp/hoxo_m/20140731/p1)
+ [RStudio Shiny チュートリアル レッスン1 ようこそ Shiny へ - Qiita](http://qiita.com/hoxo_m/items/c8365117f3444fb51df4)
+ [Community-Detection](http://glimmer.rstudio.com/andeek/gravicom/)
+ [Simple Line Chart - NVD3.js](http://nvd3.org/examples/line.html)
+ [Shiny公式チュートリアル](http://shiny.rstudio.com/tutorial/)

### 拙ブログ

+ [Shinyで確率分布を動かして遊べるページ作った](http://ksmzn.hatenablog.com/entry/statdist-shiny)
