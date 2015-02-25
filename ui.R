library(shiny)
library(shinydashboard)
library(rmarkdown)

pageTitle <- "確率分布いろいろ"

# JavaScript Files
jsFiles <- tags$head(
  tags$script(src="js/google-analytics.js")
)

# Panel for Distributions
distPanel <- function(name, en) {
  if (missing(en)) {
    box(width = 5,
        status = "primary",
        title = name,
        "参考 : ",
      a(target = "_blank",
        href = paste0('http://ja.wikipedia.org/wiki/', name),
        'Wikipedia',
        img(src = 'img/external.png')
      )
    )
  } else {
    box(width = 5,
        status = "primary",
        title = name,
        "参考 : ",
      a(target = "_blank",
        href = paste0('http://en.wikipedia.org/wiki/', en),
        'Wikipedia',
        img(src = 'img/external.png')
      )
    )
  }
}

header <- dashboardHeader(title = pageTitle)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("連続分布", icon = icon("line-chart"),
      menuSubItem("正規分布", tabName = "norm"),
      menuSubItem("アーラン分布", tabName = "erlang"),
      menuSubItem("F分布", tabName = "f"),
      menuSubItem("非心F分布", tabName = "ncf"),
      menuSubItem("カイ二乗分布", tabName = "chisq"),
      menuSubItem("非心カイ二乗分布", tabName = "ncChisq"),
      menuSubItem("ガンマ分布", tabName = "gamma"),
      menuSubItem("コーシー分布", tabName = "cauchy"),
      menuSubItem("指数分布", tabName = "exp"),
      menuSubItem("対数正規分布", tabName = "lnormal"),
      menuSubItem("t分布", tabName = "t"),
      menuSubItem("非心t分布", tabName = "nct"),
      menuSubItem("ベータ分布", tabName = "beta"),
      menuSubItem("非心ベータ分布", tabName = "ncbeta"),
      menuSubItem("連続一様分布", tabName = "unif"),
      menuSubItem("ロジスティック分布", tabName = "logis"),
      menuSubItem("ワイブル分布", tabName = "weibull")
    ),
    menuItem("離散分布", icon = icon("bar-chart-o"),
      menuSubItem("幾何分布", tabName = "geom"),
      menuSubItem("超幾何分布", tabName = "hyper"),
      menuSubItem("二項分布", tabName = "binom"),
      menuSubItem("負の二項分布", tabName = "nbinom"),
      menuSubItem("ポアソン分布", tabName = "pois"),
      menuSubItem("離散一様分布", tabName = "dunif")
    ),
    menuItem("About", icon = icon("info"),
      tabName = "about"
    ),
    menuItem("English", icon = icon("external-link"),
      href = "https://kaz-yos.shinyapps.io/ShinyDistributionsApp/"
    ),
    menuItem("Source code for app", icon = icon("github"),
      href = "http://github.com/ksmzn/ShinyDistributionsApp"
    ),
    tags$li(
      a( href = "http://twitter.com/intent/tweet?text=いろいろな確率分布のパラメータをいじくるアプリ&url=https://ksmzn.shinyapps.io/statdist/&via=ksmzn&hashtags=shiny",
        target = "_blank",
        icon("twitter"),
        onClick = "window.open(encodeURI(decodeURI(this.href)),
          'tweetwindow',
          'width=550, height=450, personalbar=0, toolbar=0, scrollbars=1, resizable=1'
          ); return false;",
        span('Tweet'),
        tags$small(
          class = paste0("badge pull-right bg-", 'light-blue'),
          'Share'
        )
      )
    ),
    tags$li(
      a( href = "http://www.facebook.com/sharer.php?u=https://ksmzn.shinyapps.io/statdist/&t=いろいろな確率分布のパラメータをいじくるアプリ",
        target = "_blank",
        icon("facebook"),
        span('Facebook'),
        tags$small(
          class = paste0("badge pull-right bg-", 'light-blue'),
          'Share'
        )
      )
    ),
    menuItem("@ksmzn", icon = icon("twitter"),
      href = "https://twitter.com/ksmzn"
    ),
    menuItem("Blog", icon = icon("pencil"),
      href = "http://ksmzn.hatenablog.com/"
    )
  )
)

board.about <- tabItem(tabName = "about",
  fluidRow(
    column(12,
      includeMarkdown("about.md")
    )
  )
)
####################################################
# Tab Items for Distributions
####################################################
# 連続分布
board.norm <- tabItem(tabName = "norm",
  fluidRow(
    distPanel("正規分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f(x)=\\frac{1}{\\sqrt{2\\pi\\sigma^{2}}}
        \\exp\\!\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2} \\right)
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("norm", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("norm", "range", sep="."), "範囲",
        min = -100, max = 100, value = c(-10, 10), step= 1
      ),
      sliderInput(paste("norm", "mean", sep="."), "平均 \\(\\mu\\)",
        min = -50, max = 50, value = 0, step= 1
      ),
      sliderInput(paste("norm", "sd", sep="."), "標準偏差 \\(\\sigma\\)",
        min = 0, max = 10, value = 1, step= 0.5
      )
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("normalPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("normal.meanBox", width = 6),
    valueBoxOutput("normal.varianceBox", width = 6)
  )
)

board.erlang <- tabItem(tabName = "erlang",
  fluidRow(
    distPanel("アーラン分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f(x; n, \\lambda)=
        {\\lambda^{n} x^{n-1} e^{-\\lambda x} \\over (n-1)!}\\quad\\mbox{for }x>0
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("erlang", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("erlang", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("erlang", "shape", sep="."), "\\(n\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("erlang", "scale", sep="."), "\\(\\lambda\\)",
        min = 0.5, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("erlangPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("erlang.meanBox", width = 6),
    valueBoxOutput("erlang.varianceBox", width = 6)
  )
)

board.f <- tabItem(tabName = "f",
  fluidRow(
    distPanel("F分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f(x) = \\frac{1}{\\mathrm{B}(d_1/2, d_2/2)} \\; 
        \\left(\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_1/2} \\; 
        \\left(1-\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_2/2} \\; x^{-1}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("f", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("f", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("f", "df1", sep="."), "自由度 \\(d_1\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("f", "df2", sep="."), "自由度 \\(d_2\\)",
        min = 1, max = 20, value = 1, step= 1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("fPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("f.meanBox", width = 6),
    valueBoxOutput("f.varianceBox", width = 6)
  )
)

board.ncf <- tabItem(tabName = "ncf",
  fluidRow(
    distPanel("非心F分布", "Noncentral_F-distribution"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
      f(x) =\\sum\\limits_{k=0}^\\infty
      \\frac{e^{-\\lambda/2}(\\lambda/2)^k}
      { B\\left(\\frac{\\nu_2}{2},\\frac{\\nu_1}{2}+k\\right) k!}
      \\left(\\frac{\\nu_1}{\\nu_2}\\right)^{\\frac{\\nu_1}{2}+k}
      \\left(\\frac{\\nu_2}{\\nu_2+\\nu_1x}\\right)
      ^{\\frac{\\nu_1+\\nu_2}{2}+k}x^{\\nu_1/2-1+k}
      \\ \\ \\ \\ \\mathrm{for\\ } x > 0
$$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("ncf", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("ncf", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("ncf", "df1", sep="."), "自由度 \\(\\nu_1\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("ncf", "df2", sep="."), "自由度 \\(\\nu_2\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("ncf", "ncp", sep="."), "非中心度 \\(\\lambda\\)",
        min = 0, max = 20, value = 0, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("ncfPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("ncf.meanBox", width = 12),
    valueBoxOutput("ncf.varianceBox", width = 12)
  )
)

board.chisq <- tabItem(tabName = "chisq",
  fluidRow(
    distPanel("カイ二乗分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f(x;k)=\\frac{(1/2)^{k/2}}{\\Gamma(k/2)} x^{k/2 - 1} e^{-x/2}
        \\ \\ \\ \\ \\mathrm{for\\ } x > 0
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("chisq", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("chisq", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("chisq", "df", sep="."), "自由度 \\(k\\)",
        min = 1, max = 20, value = 1, step= 1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("chisqPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("chisq.meanBox", width = 6),
    valueBoxOutput("chisq.varianceBox", width = 6)
  )
)

board.ncChisq <- tabItem(tabName = "ncChisq",
  fluidRow(
    distPanel("非心カイ二乗分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f_X(x; k,\\lambda) =
        \\sum_{i=0}^\\infty \\frac{e^{-\\lambda/2} (\\lambda/2)^i}{i!} f_{Y_{k+2i}}(x)
        \\ \\ \\ \\ \\mathrm{for\\ } x > 0\\\\
        \\\\ Y_q \\mathrm{\\ は自由度\\ } q \\mathrm{\\ のカイ二乗分布に従う\\ } 
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("ncChisq", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("ncChisq", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("ncChisq", "df", sep="."), "自由度 \\(k\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("ncChisq", "ncp", sep="."), "非中心度 \\(\\lambda\\)",
        min = 0, max = 20, value = 0, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("ncChisqPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("ncChisq.meanBox", width = 6),
    valueBoxOutput("ncChisq.varianceBox", width = 6)
  )
)

board.gamma <- tabItem(tabName = "gamma",
  fluidRow(
    distPanel("ガンマ分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f(x) = x^{k-1} \\frac{e^{-x/\\theta}}{\\Gamma(k)\\,\\theta^k}
        \\ \\ \\ \\ \\mathrm{for\\ } x > 0
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("gamma", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("gamma", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("gamma", "shape", sep="."), "形状 \\(k\\)",
        min = 0, max = 20, value = 1, step= 0.1),
      sliderInput(paste("gamma", "scale", sep="."), "尺度 \\(\\theta\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("gammaPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("gamma.meanBox", width = 6),
    valueBoxOutput("gamma.varianceBox", width = 6)
  )
)

board.cauchy <- tabItem(tabName = "cauchy",
  fluidRow(
    distPanel("コーシー分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        \\begin{align}
        f(x; x_0,\\gamma) &=
        { 1 \\over \\pi } \\left[ { \\gamma \\over (x - x_0)^2 + \\gamma^2  } \\right]
        \\end{align}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("cauchy", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("cauchy", "range", sep="."), "範囲",
        min = -100, max = 100, value = c(-10, 10), step= 0.5),
      sliderInput(paste("cauchy", "location", sep="."), "位置 \\(x_0\\)",
        min = -20, max = 20, value = 0, step= 0.1),
      sliderInput(paste("cauchy", "scale", sep="."), "尺度 \\(\\gamma\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("cauchyPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("cauchy.meanBox", width = 6),
    valueBoxOutput("cauchy.varianceBox", width = 6)
  )
)

board.exp <- tabItem(tabName = "exp",
  fluidRow(
    distPanel("指数分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
      f(x; \\lambda) = \\left\\{
        \\begin{array}{ll}
        \\lambda e^{-\\lambda x} & (x \\geq 0) \\\\ 0 & (x < 0)
        \\end{array}
        \\right.
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("exp", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("exp","range",sep="."), "範囲",
        min = -10, max = 50, value = c(0, 5), step= 1),
      sliderInput(paste("exp","rate",sep="."), "\\(\\lambda\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("expPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("exp.meanBox", width = 6),
    valueBoxOutput("exp.varianceBox", width = 6)
  )
)

board.lnormal <- tabItem(tabName = "lnormal",
  fluidRow(
    distPanel("対数正規分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
      f(x) = \\frac{1}{\\sqrt{2\\pi} \\sigma x} e^{-\\frac{ (\\ln{x}-\\mu)^2}{2\\sigma^2} },
      \\quad 0<x< \\infty
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("lnormal", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("lnormal", "range", sep="."), "範囲",
        min = 0, max = 200, value = c(0, 20), step= 0.5),
      sliderInput(paste("lnormal", "meanlog", sep="."), "平均log",
        min = -30, max = 30, value = 0, step= 0.05),
      sliderInput(paste("lnormal", "sdlog", sep="."), "標準偏差log",
        min = 0, max = 10, value = 1, step= 0.05)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("lnormalPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("lnormal.meanBox", width = 6),
    valueBoxOutput("lnormal.varianceBox", width = 6)
  )
)

board.t <- tabItem(tabName = "t",
  fluidRow(
    distPanel("t分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
      f(x) = \\frac{\\Gamma((\\nu+1)/2)}{\\sqrt{\\nu\\pi\\,}\\,
      \\Gamma(\\nu/2)} (1+x^2/\\nu)^{-(\\nu+1)/2}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("t", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("t", "range", sep="."), "範囲",
        min = -100, max = 100, value = c(-10, 10), step= 0.5),
      sliderInput(paste("t", "df", sep="."), "自由度 \\(\\nu\\)",
        min = 1, max = 20, value = 1, step= 1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("tPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("t.meanBox", width = 6),
    valueBoxOutput("t.varianceBox", width = 6)
  )
)

board.nct <- tabItem(tabName = "nct",
  fluidRow(
    distPanel("非心t分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
      f(x) =\\frac{\\nu^{\\frac{\\nu}{2}}
      \\exp\\left (-\\frac{\\nu\\mu^2}{2(x^2+\\nu)} \\right )}
      {\\sqrt{\\pi}\\Gamma(\\frac{\\nu}{2})2^{\\frac{\\nu-1}{2}}(x^2+\\nu)^{\\frac{\\nu+1}{2}}}
      \\int_0^\\infty y^\\nu\\exp\\left (-\\frac{1}{2}\\left(y-\\frac{\\mu x}{\\sqrt{x^2+\\nu}}
      \\right)^2\\right ) dy
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("nct", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("nct", "range", sep="."), "範囲",
        min = -100, max = 100, value = c(-10, 10), step= 0.5),
      sliderInput(paste("nct", "df", sep="."), "自由度 \\(\\nu\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("nct", "ncp", sep="."), "非中心度 \\(\\mu\\)",
        min = 0, max = 20, value = 0, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("nctPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("nct.meanBox", width = 6),
    valueBoxOutput("nct.varianceBox", width = 6)
  )
)

board.beta <- tabItem(tabName = "beta",
  fluidRow(
    distPanel("ベータ分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
      f(x)=\\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha,\\beta)}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("beta", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("beta", "range", sep="."), "範囲",
        min = 0, max = 1, value = c(0, 1), step= 0.01),
      sliderInput(paste("beta", "shape1", sep="."), "形状 \\(\\alpha\\)",
        min = 0, max = 20, value = 2, step= 0.1),
      sliderInput(paste("beta", "shape2", sep="."), "形状 \\(\\beta\\)",
        min = 0, max = 20, value = 2, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("betaPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("beta.meanBox", width = 6),
    valueBoxOutput("beta.varianceBox", width = 6)
  )
)

board.ncbeta <- tabItem(tabName = "ncbeta",
  fluidRow(
    distPanel("非心ベータ分布", 'Noncentral_beta_distribution'),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f(x) = \\sum_{j=0}^\\infty \\frac{1}{j!}
        \\left(\\frac{\\lambda}{2}\\right)^je^{-\\lambda/2}
        \\frac{x^{\\alpha+j-1}(1-x)^{\\beta-1}}{B(\\alpha+j,\\beta)}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("ncbeta", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("ncbeta", "range", sep="."), "範囲",
        min = 0, max = 1, value = c(0, 1), step= 0.01),
      sliderInput(paste("ncbeta", "shape1", sep="."), "形状 \\(\\alpha\\)",
        min = 0, max = 20, value = 2, step= 0.1),
      sliderInput(paste("ncbeta", "shape2", sep="."), "形状 \\(\\beta\\)",
        min = 0, max = 20, value = 2, step= 0.1),
      sliderInput(paste("ncbeta", "ncp", sep="."), "非中心度 \\(\\lambda\\)",
        min = 0, max = 20, value = 0, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("ncbetaPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("ncbeta.meanBox", width = 12),
    valueBoxOutput("ncbeta.varianceBox", width = 12)
  )
)

board.unif <- tabItem(tabName = "unif",
  fluidRow(
    distPanel("連続一様分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f(x)=\\begin{cases}
        \\frac{1}{b - a} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
        0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
        \\end{cases} 
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("unif", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("unif", "range", sep="."), "範囲",
        min = -50, max = 50, value = c(0, 1), step= 0.5)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("unifPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("unif.meanBox", width = 6),
    valueBoxOutput("unif.varianceBox", width = 6)
  )
)

board.logis <- tabItem(tabName = "logis",
  fluidRow(
    distPanel("ロジスティック分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f(x;\\mu,s) = \\frac{\\exp(-\\frac{x-\\mu}{s})}{s(1+\\exp(-\\frac{x-\\mu}{s}))^2}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("logis", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("logis", "range", sep="."), "範囲",
        min = -100, max = 100, value = c(-10, 10), step= 0.5),
      sliderInput(paste("logis", "location", sep="."), "位置 \\(\\mu\\)",
        min = -20, max = 20, value = 2, step= 0.1),
      sliderInput(paste("logis", "scale", sep="."), "尺度 \\(s\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("logisPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("logis.meanBox", width = 6),
    valueBoxOutput("logis.varianceBox", width = 6)
  )
)

board.weibull <- tabItem(tabName = "weibull",
  fluidRow(
    distPanel("ワイブル分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率密度関数", 
      helpText("$$
        f(t)=\\frac{m}{\\eta}\\left(\\frac{t}{\\eta}\\right)^{m-1}
        \\exp \\left\\{-\\left(\\frac{t}{\\eta}\\right)^m\\right\\}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("weibull", "p_or_c", sep="."), "",
        c("確率密度関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("weibull", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("weibull", "shape", sep="."), "形状 \\(m\\)",
        min = 0, max = 20, value = 1, step= 0.1),
      sliderInput(paste("weibull", "scale", sep="."), "尺度 \\(\\eta\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("weibullPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("weibull.meanBox", width = 12),
    valueBoxOutput("weibull.varianceBox", width = 12)
  )
)

# 離散分布
board.geom <- tabItem(tabName = "geom",
  fluidRow(
    distPanel("幾何分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率関数", 
      helpText("$$
        Pr(X = k) = p(1-p)^{k}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("geom", "p_or_c", sep="."), "",
        c("確率関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("geom", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("geom", "prob", sep="."), "成功確率 \\(p\\)",
        min = 0, max = 1, value = 0.5, step= 0.01)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("geomPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("geom.meanBox", width = 6),
    valueBoxOutput("geom.varianceBox", width = 6)
  )
)

board.hyper <- tabItem(tabName = "hyper",
  fluidRow(
    distPanel("超幾何分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率関数", 
      helpText("$$
      \\operatorname{P}(X=x)
      = \\frac{\\binom{m}{x}\\binom{n}{k-x}}{\\binom{m+n}{k}}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("hyper", "p_or_c", sep="."), "",
        c("確率関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("hyper", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("hyper", "m", sep="."), "成功状態の数 \\(m\\)",
        min = 0, max = 100, value = 50, step= 1),
      sliderInput(paste("hyper", "n", sep="."), "失敗状態の数 \\(n\\)",
        min = 0, max = 100, value = 50, step= 1),
      sliderInput(paste("hyper", "k", sep="."), "取り出す数 \\(k\\)",
        min = 0, max = 100, value = 10, step= 1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("hyperPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("hyper.meanBox", width = 6),
    valueBoxOutput("hyper.varianceBox", width = 6)
  )
)

board.binom <- tabItem(tabName = "binom",
  fluidRow(
    distPanel("二項分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率関数", 
      helpText("$$
      P[X=k]={n\\choose k}p^k(1-p)^{n-k}\\quad\\mbox{for}\\ k=0,1,2,\\dots,n
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("binom", "p_or_c", sep="."), "",
        c("確率関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("binom", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("binom", "size", sep="."), "試行回数 \\(n\\)",
        min = 0, max = 40, value = 10, step= 1),
      sliderInput(paste("binom", "prob", sep="."), "成功確率 \\(p\\)",
        min = 0, max = 1, value = 0.5, step= 0.01)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("binomPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("binom.meanBox", width = 6),
    valueBoxOutput("binom.varianceBox", width = 6)
  )
)

board.nbinom <- tabItem(tabName = "nbinom",
  fluidRow(
    distPanel("負の二項分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率関数", 
      helpText("$$
        f(x)=P(X=x) = {x-1 \\choose r-1} p^r (1-p)^{x-r}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("nbinom", "p_or_c", sep="."), "",
        c("確率関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("nbinom", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("nbinom", "size", sep="."), "成功回数 \\(r\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("nbinom", "prob", sep="."), "成功確率 \\(p\\)",
        min = 0, max = 1, value = 0.5, step= 0.01)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("nbinomPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("nbinom.meanBox", width = 6),
    valueBoxOutput("nbinom.varianceBox", width = 6)
  )
)

board.pois <- tabItem(tabName = "pois",
  fluidRow(
    distPanel("ポアソン分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率関数", 
      helpText("$$
      P(X=k)=\\frac{\\lambda^k e^{-\\lambda}}{k!}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("pois", "p_or_c", sep="."), "",
        c("確率関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("pois", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("pois", "lambda", sep="."), "\\(\\lambda\\)",
        min = 1, max = 20, value = 1, step= 0.5)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("poisPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("pois.meanBox", width = 6),
    valueBoxOutput("pois.varianceBox", width = 6)
  )
)

board.dunif <- tabItem(tabName = "dunif",
  fluidRow(
    distPanel("離散一様分布"),
    box(
      width = 7,
      status = "primary",
      title = "確率関数", 
      helpText("$$
      f(x)=\\begin{cases}
      \\frac{1}{n} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
      0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
      \\end{cases} 
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "パラメータ", status = "primary", solidHeader = TRUE,
      radioButtons(paste("dunif", "p_or_c", sep="."), "",
        c("確率関数"="p", "累積分布関数"="c")
      ),
      sliderInput(paste("dunif", "range", sep="."), "範囲",
        min = 0, max = 100, value = c(0, 20), step= 1)
    ),
    box(
      width = 7,
      title = "プロット", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("dunifPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("dunif.meanBox", width = 6),
    valueBoxOutput("dunif.varianceBox", width = 6)
  )
)




body <- dashboardBody(
  jsFiles,
  tabItems(
    # 連続分布
    board.norm,
    board.erlang,
    board.f,
    board.ncf,
    board.chisq,
    board.ncChisq,
    board.gamma,
    board.cauchy,
    board.exp,
    board.lnormal,
    board.t,
    board.nct,
    board.beta,
    board.ncbeta,
    board.unif,
    board.logis,
    board.weibull,
    # 離散分布
    board.geom,
    board.hyper,
    board.binom,
    board.nbinom,
    board.pois,
    board.dunif,
    # About
    board.about
  )
)

ui <- dashboardPage(header, sidebar, body)
