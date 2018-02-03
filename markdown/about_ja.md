## 確率分布 Viewer

様々な確率分布のカタチを見ることができるWebアプリです。  
R言語でアプリを作れる [Shiny](http://shiny.rstudio.com) で作りました。 

パラメータを変えて、確率分布のカタチがどのように変わるのか観察しましょう。  
平均値・分散の値も知ることができます。

上部のBookmarkボタンで、作成した分布の状態を保存できます。
URLをコピーすれば、シェアすることも可能です。

 **左メニュー** からお好きな確率分布を選んでください。

**【2018年2月3日 追記】**  

- ブックマーク機能を作成しました。  
右上の「Bookmark」ボタンからどうぞ。

- 言語切り替え機能を作成しました。  
右上の選択ボタンからどうぞ。  
現在対応しているのは日本語と英語です。  
その他の言語の対応は未定ですが、[PullRequest](https://github.com/ksmzn/ProbabilityDistributionsViewer/pulls) は大歓迎です。  
よろしくお願いします。

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

### 参考文献

このアプリを作る際に参考にしたページは以下です。  
特に、作成当時はShinyの日本語情報が少ないなか、
<a href="https://twitter.com/hoxo_m" target="_blank">@hoxo_m</a>さんの記事やコードはとても参考になりました。  
<a href="https://twitter.com/hoxo_m" target="_blank">@hoxo_m</a>さん、ありがとうございました。

#### Shiny

+ [Shiny Official Tutorial](http://shiny.rstudio.com/tutorial/)
+ [Shiny - Bookmarking state](https://shiny.rstudio.com/articles/bookmarking-state.html)
+ [Shiny - Advanced bookmarking](https://shiny.rstudio.com/articles/advanced-bookmarking.html)
+ [Shiny - Modularizing Shiny app code](https://shiny.rstudio.com/articles/modules.html)

#### shinydashboard

+ [shinydashboard](https://rstudio.github.io/shinydashboard/) 
+ [Shiny Dashboard Behavior](https://rstudio.github.io/shinydashboard/behavior.html)

#### shiny.i18n

+ [GitHub - Appsilon/shiny.i18n](https://github.com/Appsilon/shiny.i18n)

#### JavaScript

+ [Simple Line Chart - NVD3.js](http://nvd3.org/examples/line.html)

#### Tutorial & Example

+ [ボケて(bokete)のネタを全自動で流し見できるサイト作った - ほくそ笑む](http://d.hatena.ne.jp/hoxo_m/20140731/p1)
+ [RStudio Shiny チュートリアル レッスン1 ようこそ Shiny へ - Qiita](http://qiita.com/hoxo_m/items/c8365117f3444fb51df4)

### 拙ブログ

+ [Shinyで確率分布を動かして遊べるページ作った](http://ksmzn.hatenablog.com/entry/statdist-shiny)
+ [Shinyで作った確率分布を動かせるページを, NVD3.jsでヌルヌルでインタラクティブなグラフにしました。そしてShinyでD3.jsを使う方法３つ。](http://ksmzn.hatenablog.com/entry/shiny-nvd3-js-nuru) 
+ [確率分布を学ぶアプリを、shinydashboard を使って新しくしてみた & 英訳していただきました。](http://ksmzn.hatenablog.com/entry/shiny-dashboard-english)
+ [Dockerを使ってDigitalOceanにShinyアプリを公開する](http://ksmzn.hatenablog.com/entry/degitalocean-shiny-docker)
+ [shiny.i18nパッケージでshinyを多言語対応](http://ksmzn.hatenablog.com/entry/shiny-i18n)
+ [「ShinyModule」で中規模Shinyアプリをキレイにする](http://ksmzn.hatenablog.com/entry/shiny-module)


