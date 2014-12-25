library(shiny)

# アプリケーションの UI 定義。ヒストグラムを描く
shinyUI(
  navbarPage("確率分布",
    tabPanel("Home",
        titlePanel("正規分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("home", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("home", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("home", "mean", sep="."), "平均",
                        min = -50, max = 50, value = 0, step= 0.05),
            sliderInput(paste("home", "sd", sep="."), "標準偏差",
                        min = 0, max = 10, value = 1, step= 0.05)
          ),
          mainPanel(
            plotOutput("homePlot")
          )
        )
    ),
    navbarMenu("連続分布",
#       tabPanel("アーラン分布",
#       ),
#       tabPanel("一般化双曲型分布"),
#       tabPanel("ウィッシャート分布"),
      tabPanel("F分布",
        titlePanel("F分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("f", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("f", "range", sep="."), "範囲",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("f", "df1", sep="."), "自由度1",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("f", "df2", sep="."), "自由度2",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("f", "ncp", sep="."), "非中心度パラメータ",
                        min = 0, max = 20, value = 0, step= 0.1)
          ),
          mainPanel(
            plotOutput("fPlot")
          )
        )
      ),
      tabPanel("カイ二乗分布",
        titlePanel("カイ二乗分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("chisq", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("chisq", "range", sep="."), "範囲",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("chisq", "df", sep="."), "自由度",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("chisq", "ncp", sep="."), "非中心度パラメータ",
                        min = 0, max = 20, value = 0, step= 0.1)
          ),
          mainPanel(
            plotOutput("chisqPlot")
          )
        )
               ),
#       tabPanel("ガンベル分布"),
      tabPanel("ガンマ分布",
        titlePanel("ガンマ分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("gamma", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("gamma", "range", sep="."), "範囲",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("gamma", "shape", sep="."), "形状",
                        min = 0, max = 20, value = 1, step= 0.1),
            sliderInput(paste("gamma", "scale", sep="."), "尺度",
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
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("cauchy", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("cauchy", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("cauchy", "location", sep="."), "位置",
                        min = -20, max = 20, value = 0, step= 0.1),
            sliderInput(paste("cauchy", "scale", sep="."), "尺度",
                        min = 0, max = 20, value = 1, step= 0.1)
          ),
          mainPanel(
            plotOutput("cauchyPlot")
          )
        )
      ),
      tabPanel("指数分布",
        titlePanel("指数分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("exp", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("exp","range",sep="."), "範囲",
                        min = -10, max = 100, value = c(0, 5), step= 0.5),
            sliderInput(paste("exp","rate",sep="."), "λ",
                        min = 0, max = 50, value = 1, step= 0.1)
          ),
          mainPanel(
            plotOutput("expPlot")
          )
        )
      ),
      tabPanel("正規分布",
        titlePanel("正規分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("norm", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("norm", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("norm", "mean", sep="."), "平均",
                        min = -50, max = 50, value = 0, step= 0.05),
            sliderInput(paste("norm", "sd", sep="."), "標準偏差",
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
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("t", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("t", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("t", "df", sep="."), "自由度",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("t", "ncp", sep="."), "非中心度パラメータ",
                        min = 0, max = 20, value = 0, step= 0.1)
          ),
          mainPanel(
            plotOutput("tPlot")
          )
        )
      ),
#       tabPanel("ディリクレ分布"),
#       tabPanel("パレート分布"),
      tabPanel("ベータ分布",
        titlePanel("ベータ分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("beta", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("beta", "range", sep="."), "範囲",
                        min = 0, max = 1, value = c(0, 1), step= 0.01),
            sliderInput(paste("beta", "shape1", sep="."), "形状α",
                        min = 0, max = 20, value = 2, step= 0.1),
            sliderInput(paste("beta", "shape2", sep="."), "形状β",
                        min = 0, max = 20, value = 2, step= 0.1),
            sliderInput(paste("beta", "ncp", sep="."), "非中心度パラメータ",
                        min = 0, max = 20, value = 0, step= 0.1)
          ),
          mainPanel(
            plotOutput("betaPlot")
          )
        )
      ),
#       tabPanel("ラプラス分布"),
#       tabPanel("レイリー分布"),
#       tabPanel("レヴィ分布"),
      tabPanel("連続一様分布",
        titlePanel("連続一様分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("unif", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("unif", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(0, 1), step= 0.5)
#             sliderInput(paste("unif", "min", sep="."), "最小値",
#                         min = -100, max = 100, value = 0, step= 0.5),
#             sliderInput(paste("unif", "max", sep="."), "最大値",
#                         min = -100, max = 100, value = 1, step= 0.5)
          ),
          mainPanel(
            plotOutput("unifPlot")
          )
        )
      ),
      tabPanel("ロジスティック分布",
        titlePanel("ロジスティック分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("logis", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("logis", "range", sep="."), "範囲",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("logis", "location", sep="."), "位置",
                        min = -20, max = 20, value = 2, step= 0.1),
            sliderInput(paste("logis", "scale", sep="."), "尺度",
                        min = 0, max = 20, value = 1, step= 0.1)
          ),
          mainPanel(
            plotOutput("logisPlot")
          )
        )
               ),
      tabPanel("ワイブル分布",
        titlePanel("ワイブル分布"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("weibull", "p_or_c", sep="."), "",
                      c("確率密度関数"="p", "累積分布関数"="c")
            ),
            sliderInput(paste("weibull", "range", sep="."), "範囲",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("weibull", "shape", sep="."), "形状m",
                        min = 0, max = 20, value = 1, step= 0.1),
            sliderInput(paste("weibull", "scale", sep="."), "尺度η",
                        min = 0, max = 20, value = 1, step= 0.1)
          ),
          mainPanel(
            plotOutput("weibullPlot")
          )
        )
      )
    ),
    navbarMenu("離散分布",
      tabPanel("幾何分布"),
      tabPanel("超幾何分布"),
      tabPanel("ジップ分布"),
      tabPanel("多項分布"),
      tabPanel("二項分布"),
      tabPanel("負の二項分布"),
      tabPanel("ポアソン二項分布"),
      tabPanel("ベルヌーイ分布"),
      tabPanel("ポアソン分布"),
      tabPanel("離散一様分布")
      )
    )
)
