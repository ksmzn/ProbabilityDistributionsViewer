library(shiny)
library(shinydashboard)
library(rmarkdown)

pageTitle <- "ShinyDistributionsApp"
sidebarWidth <- 300

# JavaScript Files
jsFiles <- tags$head(
  tags$script(src="js/google-analytics.js")
)

####################################################
# UI ----
####################################################
header <- dashboardHeader(
  title = pageTitle,
  titleWidth = sidebarWidth,
  uiOutput("language_selector")
)

sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenuOutput("sidebar_menu")
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
    # Continuous
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
    # Discrete
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
