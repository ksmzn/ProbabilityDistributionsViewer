library(shiny)

# アプリケーションの UI 定義。ヒストグラムを描く
shinyUI(
  navbarPage("確率分布",
    tabPanel("Home",
        titlePanel("正規分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f(x)=\\frac{1}{\\sqrt{2\\pi\\sigma^{2}}} \\exp\\!\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2} \\right)$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("home", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("home", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("home", "mean", sep="."), "平均 \\(\\mu\\)",
                        min = -50, max = 50, value = 0, step= 0.05),
            sliderInput(paste("home", "sd", sep="."), "標準偏差 \\(\\sigma\\)",
                        min = 0, max = 10, value = 1, step= 0.05)
          ),
          mainPanel(
            plotOutput("homePlot")
          )
        )
    ),
    navbarMenu("連続分布",
      tabPanel("アーラン分布",
        titlePanel("アーラン分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$
              f(x; n, \\lambda)=
              {\\lambda^{n} x^{n-1} e^{-\\lambda x} \\over (n-1)!}\\quad\\mbox{for }x>0
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("erlang", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("erlang", "range", sep="."), "範囲",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("erlang", "shape", sep="."), "\\(n\\)",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("erlang", "scale", sep="."), "\\(\\lambda\\)",
                        min = 0, max = 20, value = 1, step= 0.1)
          ),
          mainPanel(
            plotOutput("erlangPlot")
          )
        )
      ),
#       tabPanel("一般化双曲型分布"),
#       tabPanel("ウィッシャート分布"),
      tabPanel("F分布",
        titlePanel("F分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$ f(x) = \\frac{1}{\\mathrm{B}(d_1/2, d_2/2)} \\; \\left(\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_1/2} \\; \\left(1-\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_2/2} \\; x^{-1} $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("fPlot")
          )
        )
      ),
      tabPanel("非心F分布",
        titlePanel("非心F分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$ f(x)
              =\\sum\\limits_{k=0}^\\infty
              \\frac{e^{-\\lambda/2}(\\lambda/2)^k}
              { B\\left(\\frac{\\nu_2}{2},\\frac{\\nu_1}{2}+k\\right) k!}
              \\left(\\frac{\\nu_1}{\\nu_2}\\right)^{\\frac{\\nu_1}{2}+k}
              \\left(\\frac{\\nu_2}{\\nu_2+\\nu_1x}\\right)
              ^{\\frac{\\nu_1+\\nu_2}{2}+k}x^{\\nu_1/2-1+k}
              \\ \\ \\ \\ \\mathrm{for\\ } x > 0
              $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("ncfPlot")
          )
        )
      ),
      tabPanel("カイ二乗分布",
        titlePanel("カイ二乗分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f(x;k)=\\frac{(1/2)^{k/2}}{\\Gamma(k/2)} x^{k/2 - 1} e^{-x/2}
              \\ \\ \\ \\ \\mathrm{for\\ } x > 0$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("chisq", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("chisq", "range", sep="."), "範囲",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("chisq", "df", sep="."), "自由度 \\(k\\)",
                        min = 1, max = 20, value = 1, step= 1)
          ),
          mainPanel(
            plotOutput("chisqPlot")
          )
        )
               ),
      tabPanel("非心カイ二乗分布",
        titlePanel("非心カイ二乗分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f_X(x; k,\\lambda) =
              \\sum_{i=0}^\\infty \\frac{e^{-\\lambda/2} (\\lambda/2)^i}{i!} f_{Y_{k+2i}}(x)
              \\ \\ \\ \\ \\mathrm{for\\ } x > 0\\\\
              \\\\ Y_q \\mathrm{\\ は自由度\\ } q \\mathrm{\\ のカイ二乗分布に従う\\ } $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("ncChisqPlot")
          )
        )
               ),
#       tabPanel("ガンベル分布"),
      tabPanel("ガンマ分布",
        titlePanel("ガンマ分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f(x) = x^{k-1} \\frac{e^{-x/\\theta}}{\\Gamma(k)\\,\\theta^k}
                     \\ \\ \\ \\ \\mathrm{for\\ } x > 0$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("gammaPlot")
          )
        )
               ),
#       tabPanel("逆ガウス分布"),
      tabPanel("コーシー分布",
        titlePanel("コーシー分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$\\begin{align}f(x; x_0,\\gamma) &= { 1 \\over \\pi } \\left[ { \\gamma \\over (x - x_0)^2 + \\gamma^2  } \\right]\\end{align}$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("cauchyPlot")
          )
        )
      ),
      tabPanel("指数分布",
        titlePanel("指数分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f(x; \\lambda) = \\left\\{ \\begin{array}{ll} \\lambda e^{-\\lambda x} & (x \\geq 0) \\\\ 0 & (x < 0)\\end{array}\\right.$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("exp", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("exp","range",sep="."), "範囲",
                        min = -10, max = 100, value = c(0, 5), step= 0.5),
            sliderInput(paste("exp","rate",sep="."), "\\(\\lambda\\)",
                        min = 0, max = 50, value = 1, step= 0.1)
          ),
          mainPanel(
            plotOutput("expPlot")
          )
        )
      ),
      tabPanel("正規分布",
        titlePanel("正規分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f(x)=\\frac{1}{\\sqrt{2\\pi\\sigma^{2}}} \\exp\\!\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2} \\right)$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("norm", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("norm", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("norm", "mean", sep="."), "平均 \\(\\mu\\)",
                        min = -50, max = 50, value = 0, step= 0.05),
            sliderInput(paste("norm", "sd", sep="."), "標準偏差 \\(\\sigma\\)",
                        min = 0, max = 10, value = 1, step= 0.05)
          ),
          mainPanel(
            plotOutput("normalPlot")
          )
        )
      ),
#       tabPanel("双曲線正割分布"),
      tabPanel("対数正規分布",
        titlePanel("対数正規分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f(x) = \\frac{1}{\\sqrt{2\\pi} \\sigma x} e^{-\\frac{ (\\ln{x}-\\mu)^2}{2\\sigma^2} }, \\quad 0<x< \\infty$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("lnorm", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("lnorm", "range", sep="."), "範囲",
                        min = 0, max = 200, value = c(0, 20), step= 0.5),
            sliderInput(paste("lnorm", "meanlog", sep="."), "平均log",
                        min = -50, max = 50, value = 0, step= 0.05),
            sliderInput(paste("lnorm", "sdlog", sep="."), "標準偏差log",
                        min = 0, max = 10, value = 1, step= 0.05)
          ),
          mainPanel(
            plotOutput("lnormalPlot")
          )
        )
      ),
      tabPanel("t分布",
        titlePanel("t分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f(x) = \\frac{\\Gamma((\\nu+1)/2)}{\\sqrt{\\nu\\pi\\,}\\,\\Gamma(\\nu/2)} (1+x^2/\\nu)^{-(\\nu+1)/2}$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("t", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("t", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("t", "df", sep="."), "自由度 \\(\\nu\\)",
                        min = 1, max = 20, value = 1, step= 1)
          ),
          mainPanel(
            plotOutput("tPlot")
          )
        )
      ),
      tabPanel("非心t分布",
        titlePanel("非心t分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$ f(x) =\\frac{\\nu^{\\frac{\\nu}{2}} \\exp\\left (-\\frac{\\nu\\mu^2}{2(x^2+\\nu)} \\right )}{\\sqrt{\\pi}\\Gamma(\\frac{\\nu}{2})2^{\\frac{\\nu-1}{2}}(x^2+\\nu)^{\\frac{\\nu+1}{2}}} \\int_0^\\infty y^\\nu\\exp\\left (-\\frac{1}{2}\\left(y-\\frac{\\mu x}{\\sqrt{x^2+\\nu}}\\right)^2\\right ) dy$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("nctPlot")
          )
        )
      ),
#       tabPanel("ディリクレ分布"),
#       tabPanel("パレート分布"),
      tabPanel("ベータ分布",
        titlePanel("ベータ分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f(x)=\\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha,\\beta)}$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("betaPlot")
          )
        )
      ),
      tabPanel("非心ベータ分布",
        titlePanel("非心ベータ分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$
              f(x) = \\sum_{j=0}^\\infty \\frac{1}{j!}
              \\left(\\frac{\\lambda}{2}\\right)^je^{-\\lambda/2}
              \\frac{x^{\\alpha+j-1}(1-x)^{\\beta-1}}{B(\\alpha+j,\\beta)}
            $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("ncbetaPlot")
          )
        )
      ),
#       tabPanel("ラプラス分布"),
#       tabPanel("レイリー分布"),
#       tabPanel("レヴィ分布"),
      tabPanel("連続一様分布",
        titlePanel("連続一様分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$
  f(x)=\\begin{cases}
  \\frac{1}{b - a} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
  0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
  \\end{cases} 
            $$")
#             helpText("$$
#               f(x)=\\left\\{\\begin{matrix}
#               \\frac{1}{b - a} & \\ \\ \\ \\mathrm{for}\\ a \\le x \\le b, \\\\  \\\\
#               0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b, \\end{matrix}\\right
#             $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("unif", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("unif", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(0, 1), step= 0.5)
          ),
          mainPanel(
            plotOutput("unifPlot")
          )
        )
      ),
      tabPanel("ロジスティック分布",
        titlePanel("ロジスティック分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$
              f(x;\\mu,s) = \\frac{\\exp(-\\frac{x-\\mu}{s})}{s(1+\\exp(-\\frac{x-\\mu}{s}))^2}
            $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("logisPlot")
          )
        )
               ),
      tabPanel("ワイブル分布",
        titlePanel("ワイブル分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$
                     f(t)=\\frac{m}{\\eta}\\left(\\frac{t}{\\eta}\\right)^{m-1}
                     \\exp \\left\\{-\\left(\\frac{t}{\\eta}\\right)^m\\right\\}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("weibullPlot")
          )
        )
      )
    ),
    navbarMenu("離散分布",
      tabPanel("幾何分布",
        titlePanel("幾何分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$\\Pr(X = k) = p(1-p)^{k}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("geom", "p_or_c", sep="."), "",
                      c("確率関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("geom", "range", sep="."), "範囲",
                        min = 0, max = 100, value = c(0, 20), step= 1),
            sliderInput(paste("geom", "prob", sep="."), "成功確率 \\(p\\)",
                        min = 0, max = 1, value = 0.5, step= 0.01)
          ),
          mainPanel(
            plotOutput("geomPlot")
          )
        )
      ),
      tabPanel("超幾何分布",
        titlePanel("超幾何分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$
              \\operatorname{P}(X=x)
              = \\frac{\\binom{m}{x}\\binom{n}{k-x}}{\\binom{m+n}{k}}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("hyperPlot")
          )
        )
      ),
#       tabPanel("ジップ分布",),
#       tabPanel("多項分布",),
      tabPanel("二項分布",
        titlePanel("二項分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$P[X=k]={n\\choose k}p^k(1-p)^{n-k}\\quad\\mbox{for}\\ k=0,1,2,\\dots,n 
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("binomPlot")
          )
        )
      ),
      tabPanel("負の二項分布",
        titlePanel("負の二項分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$f(x)=P(X=x) = {x-1 \\choose r-1} p^r (1-p)^{x-r}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
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
          mainPanel(
            plotOutput("nbinomPlot")
          )
        )
      ),
#       tabPanel("ポアソン二項分布",),
#       tabPanel("ベルヌーイ分布",),
      tabPanel("ポアソン分布",
        titlePanel("ポアソン分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$P(X=k)=\\frac{\\lambda^k e^{-\\lambda}}{k!}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("pois", "p_or_c", sep="."), "",
                      c("確率関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("pois", "range", sep="."), "範囲",
                        min = 0, max = 100, value = c(0, 20), step= 1),
            sliderInput(paste("pois", "lambda", sep="."), "\\(\\lambda\\)",
                        min = 1, max = 20, value = 1, step= 0.5)
          ),
          mainPanel(
            plotOutput("poisPlot")
          )
        )
      ),
      tabPanel("離散一様分布",
        titlePanel("離散一様分布"),
        fluidRow(
          column(12,
            withMathJax(),
            helpText("$$
              f(x)=\\begin{cases}
              \\frac{1}{n} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
              0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
              \\end{cases} 
            $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("dunif", "p_or_c", sep="."), "",
                      c("確率関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("dunif", "range", sep="."), "範囲",
                        min = 0, max = 100, value = c(0, 20), step= 1)
            ),
          mainPanel(
            plotOutput("dunifPlot")
          )
        )
      )
    )
  )
)
