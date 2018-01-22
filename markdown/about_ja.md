## いろいろな確率分布のパラメータをいじくるアプリ

いろいろな確率分布のカタチを見ることができるWebアプリです。  
パラメータをいじくって、確率分布のカタチがどのように変わるのか観察しましょう。  
 **左メニュー** からお好きな確率分布を選んでください。

このアプリはR言語のWebアプリフレームワークである<a href="http://shiny.rstudio.com/" target="_blank">Shiny</a>
で、<a href="https://twitter.com/ksmzn" target="_blank">@ksmzn</a>が作りました。  
ご指摘や、追加すべき確率分布などがありましたらTwitterで教えてくださると助かります。  
また、全てのコードは<a href='https://github.com/ksmzn/ShinyDistributionsApp' target="_blank">GitHub</a>にもおいてありますので、拙いコードでよろしければ参考にしてください。

**【2015年1月14日 追記】**  
NVD3.jsを使ったグラフに変更しました！  
また、分布名の隣に、日本版Wikipediaへのリンクを追加しました。

**【2015年1月27日 追記】**  

なんと、
このアプリを
<a href="https://twitter.com/kaz_yos" target="_blank">@kaz_yos</a>
さんが英語に翻訳して下さいました！！

<p><a href="https://kaz-yos.shinyapps.io/ShinyDistributionsApp/" target="_blank"><img class="alignleft" align="left" border="0" src="http://capture.heartrails.com/150x130/shadow?https://kaz-yos.shinyapps.io/ShinyDistributionsApp/" alt="" width="150" height="130" /></a><a style="color:#0070C5;" href="https://kaz-yos.shinyapps.io/ShinyDistributionsApp/" target="_blank">Shiny web app for live demonstration of probability distributions</a><a href="http://b.hatena.ne.jp/entry/https://kaz-yos.shinyapps.io/ShinyDistributionsApp/" target="_blank"><img border="0" src="http://b.hatena.ne.jp/entry/image/https://kaz-yos.shinyapps.io/ShinyDistributionsApp/" alt="" /></a><br style="clear:both;" /><br></p>

<a href="https://twitter.com/kaz_yos" target="_blank">@kaz_yos</a>
さん、ありがとうございました。

**【2015年2月25日 追記】**  

<a href='http://rstudio.github.io/shinydashboard/index.html' target="_blank">shinydashboard</a>
を使って
デザインやUIを一新しました。
shinydashboard は綺麗なデザインが簡単にできて素晴らしいですね。  

また、期待値や分散を追加しました。
より便利になったと思います。

## 参考文献

このアプリを作る際に参考にしたページは以下です。  
特に、まだまだShinyの日本語情報が少ないなか、
<a href="https://twitter.com/hoxo_m" target="_blank">@hoxo_m</a>さんの記事やコードはとても参考になりました。  
<a href="https://twitter.com/hoxo_m" target="_blank">@hoxo_m</a>さん、ありがとうございました。

+ <a href="http://d.hatena.ne.jp/hoxo_m/20140731/p1" target="_blank">ボケて(bokete)のネタを全自動で流し見できるサイト作った - ほくそ笑む</a>
+ <a href="http://qiita.com/hoxo_m/items/c8365117f3444fb51df4" target="_blank">RStudio Shiny チュートリアル レッスン1 ようこそ Shiny へ - Qiita</a>
+ <a href="http://glimmer.rstudio.com/andeek/gravicom/" target="_blank">Community-Detection</a>
+ <a href="http://nvd3.org/examples/line.html" target="_blank">Simple Line Chart - NVD3.js</a>
+ <a href="http://shiny.rstudio.com/tutorial/" target="_blank">Shiny公式チュートリアル</a>
+ <a href="http://rstudio.github.io/shinydashboard/index.html" target="_blank">shinydashboard</a>

## 拙ブログ

+ <a href="http://ksmzn.hatenablog.com/entry/statdist-shiny" target="_blank">Shinyで確率分布を動かして遊べるページ作った</a>
+ <a href="http://ksmzn.hatenablog.com/entry/shiny-nvd3-js-nuru" target="_blank">Shinyで作った確率分布を動かせるページを, NVD3.jsでヌルヌルでインタラクティブなグラフにしました。そしてShinyでD3.jsを使う方法３つ。</a>
