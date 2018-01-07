library(shiny)
library(shinydashboard)
library(rmarkdown)

pageTitle <- "確率分布いろいろ"

# JavaScript Files
jsFiles <- tags$head(
  tags$script(src="js/google-analytics.js")
)

####################################################
# UI ----
####################################################
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
      menuSubItem("指数分布", tabName = "exp_dist"),
      menuSubItem("対数正規分布", tabName = "lnormal"),
      menuSubItem("t分布", tabName = "t_dist"),
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
      a( href = "http://twitter.com/intent/tweet?text=いろいろな確率分布のパラメータをいじくるアプリ&url=http://statdist.ksmzn.com/&via=ksmzn&hashtags=rshiny",
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
      a( href = "http://www.facebook.com/sharer.php?u=http://statdist.ksmzn.com/&t=いろいろな確率分布のパラメータをいじくるアプリ",
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
# Tab Items for Distributions ----
####################################################
# 連続分布 ----
## Normal
board.norm <- distTabUI(norm)
board.erlang <- distTabUI(erlang)
board.f <- distTabUI(f)
board.ncf <- distTabUI(ncf, wide = T)
board.chisq <- distTabUI(chisq)
board.ncChisq <- distTabUI(ncChisq)
board.gamma <- distTabUI(gamma)
board.cauchy <- distTabUI(cauchy)
board.exp_dist <- distTabUI(exp_dist)
board.lnormal <- distTabUI(lnormal)
board.t_dist <- distTabUI(t_dist)
board.nct <- distTabUI(nct, wide = T)
board.beta <- distTabUI(beta)
board.ncbeta <- distTabUI(ncbeta, wide = T)
board.unif <- distTabUI(unif)
board.logis <- distTabUI(logis)
board.weibull <- distTabUI(weibull, wide = T)
# 離散分布
board.geom <- distTabUI(geom)
board.hyper <- distTabUI(hyper)
board.binom <- distTabUI(binom)
board.nbinom <- distTabUI(nbinom)
board.pois <- distTabUI(pois)
board.dunif <- distTabUI(dunif)

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
    board.exp_dist,
    board.lnormal,
    board.t_dist,
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
