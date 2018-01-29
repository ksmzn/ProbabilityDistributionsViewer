library(shiny)
library(shinydashboard)

server <- function(input, output, session) {
  ###########################################################################
  # Settings
  ###########################################################################
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })

  # Maintain Tab State
  observeEvent(input$selected_language, {
    updateTabItems(session, "tabs", input$tabs)
  })

  # Reproduce parameters when language changes
  initParams <- eventReactive(input$selected_language, {
    dist_opened <- input$tabs
    if(!(dist_opened %in% dist_names)){
      return(NULL)
    }
    ns <- NS(dist_opened)

    # App is prepairing
    if(is.null(input[[ns("p_or_c")]])){
      return(NULL)
    }

    # Keep parameters
    d <- distributions[[dist_opened]]
    param_names <- names(d$params)
    targets <- c(c("p_or_c", "range"), param_names)

    dist_params <- reactiveValuesToList(input)[ns(targets)]
    names(dist_params) <- targets

    params <- list(dist_params)
    names(params) <- dist_opened
    return(params)
  })

  # Bookmark
  onBookmarked(function(url) {
    url_filtered <- filterQueryParams(url, reactiveValuesToList(input))
    updateQueryString(url_filtered)
  })
  onBookmarked(showBookmarkModal(input, i18n))

  ###########################################################################
  # UI
  ###########################################################################
  output$language_selector <- renderUI({
    selectLanguageInput(
      inputId = 'selected_language',
      choices = i18n()$languages,
      selected = input$selected_language,
      width = "100px"
    )
  })

  output$about <- renderUI({
    fn <- paste0("markdown/", i18n()$t("about.md"))
    fluidRow(
      column(12,
        includeMarkdown(fn)
      )
    )
  })

  output$sidebar_menu <- renderMenu({
    sidebarMenu(
      id = "tabs",
      menuItem(i18n()$t("Continuous distributions"), icon = icon("line-chart"),
        menuSubItem(i18n()$t("Normal distribution"), tabName = "norm"),
        menuSubItem(i18n()$t("Erlang distribution"), tabName = "erlang"),
        menuSubItem(i18n()$t("F-distribution"), tabName = "f"),
        menuSubItem(i18n()$t("Noncentral F-distribution"), tabName = "ncf"),
        menuSubItem(i18n()$t("Chi-squared distribution"), tabName = "chisq"),
        menuSubItem(i18n()$t("Noncentral chi-squared distribution"), tabName = "ncChisq"),
        menuSubItem(i18n()$t("Gamma distribution"), tabName = "gamma"),
        menuSubItem(i18n()$t("Cauchy distribution"), tabName = "cauchy"),
        menuSubItem(i18n()$t("Exponential distribution"), tabName = "exp_dist"),
        menuSubItem(i18n()$t("Log-normal distribution"), tabName = "lnormal"),
        menuSubItem(i18n()$t("Student's t-distribution"), tabName = "t_dist"),
        menuSubItem(i18n()$t("Noncentral t-distribution"), tabName = "nct"),
        menuSubItem(i18n()$t("Beta distribution"), tabName = "beta"),
        menuSubItem(i18n()$t("Noncentral beta distribution"), tabName = "ncbeta"),
        menuSubItem(i18n()$t("Uniform distribution (continuous)"), tabName = "unif"),
        menuSubItem(i18n()$t("Logistic distribution"), tabName = "logis"),
        menuSubItem(i18n()$t("Weibull distribution"), tabName = "weibull")
      ),
      menuItem(i18n()$t("Discrete distributions"), icon = icon("bar-chart-o"),
        menuSubItem(i18n()$t("Geometric distribution"), tabName = "geom"),
        menuSubItem(i18n()$t("Hypergeometric distribution"), tabName = "hyper"),
        menuSubItem(i18n()$t("Binomial distribution"), tabName = "binom"),
        menuSubItem(i18n()$t("Negative binomial distribution"), tabName = "nbinom"),
        menuSubItem(i18n()$t("Poisson distribution"), tabName = "pois"),
        menuSubItem(i18n()$t("Discrete uniform distribution"), tabName = "dunif")
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
        a(href = paste0("http://twitter.com/intent/tweet?text=", i18n()$t("ShinyDistributionsApp"), "&url=http://statdist.ksmzn.com/&via=ksmzn&hashtags=rshiny"),
          target = "_blank",
          icon("twitter"),
          onClick = "window.open(encodeURI(decodeURI(this.href)),
            'tweetwindow',
            'width=550, height=450, personalbar=0, toolbar=0, scrollbars=1, resizable=1'
            ); return false;",
          span('Tweet'),
          tags$small(
            class = "badge pull-right bg-light-blue",
            'Share'
          )
        )
      ),
      tags$li(
        a(href = paste0("http://www.facebook.com/sharer.php?u=http://statdist.ksmzn.com/&t=", i18n()$t("ShinyDistributionsApp")),
          target = "_blank",
          icon("facebook"),
          span('Facebook'),
          tags$small(
            class = "badge pull-right bg-light-blue",
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
  })




  ###########################################################################
  # Continuous probability distributions
  ###########################################################################
  callDistributionModule(norm, initParams, i18n)
  callDistributionModule(erlang, initParams, i18n)
  callDistributionModule(f, initParams, i18n)
  callDistributionModule(ncf, initParams, i18n)
  callDistributionModule(chisq, initParams, i18n)
  callDistributionModule(ncChisq, initParams, i18n)
  callDistributionModule(gamma, initParams, i18n)
  callDistributionModule(cauchy, initParams, i18n)
  callDistributionModule(exp_dist, initParams, i18n)
  callDistributionModule(lnormal, initParams, i18n)
  callDistributionModule(t_dist, initParams, i18n)
  callDistributionModule(nct, initParams, i18n)
  callDistributionModule(beta, initParams, i18n)
  callDistributionModule(ncbeta, initParams, i18n)
  callDistributionModule(unif, initParams, i18n)
  callDistributionModule(logis, initParams, i18n)
  callDistributionModule(weibull, initParams, i18n)
  ###########################################################################
  # Discrete probability distributions
  ###########################################################################
  callDistributionModule(geom, initParams, i18n)
  callDistributionModule(hyper, initParams, i18n)
  callDistributionModule(binom, initParams, i18n)
  callDistributionModule(nbinom, initParams, i18n)
  callDistributionModule(pois, initParams, i18n)
  callDistributionModule(dunif, initParams, i18n)
}
