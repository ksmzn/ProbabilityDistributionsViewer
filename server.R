library(shiny)
library(shinydashboard)
library(purrr)

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
    if (!(dist_opened %in% dist_names)) {
      return(NULL)
    }
    ns <- NS(dist_opened)

    # App is prepairing
    if (is.null(input[[ns("p_or_c")]])) {
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
  onRestore(function(state) {
    updateTabItems(session, "tabs", state$input$tabs)
  })

  ###########################################################################
  # UI
  ###########################################################################
  output$language_selector <- renderUI({
    selectLanguageInput(
      inputId = "selected_language",
      choices = i18n()$languages,
      selected = input$selected_language,
      width = "100px"
    )
  })

  output$about <- renderUI({
    fn <- paste0("markdown/", i18n()$t("about.md"))
    fluidRow(
      column(
        12,
        includeMarkdown(fn)
      )
    )
  })

  output$sidebar_menu <- renderMenu({
    sidebarMenu(
      id = "tabs",
      menuItem(
        i18n()$t("Continuous distributions"),
        icon = icon("line-chart"),
        purrr::map(continuous_distributions, ~ {
          menuSubItem(i18n()$t(.x$name), tabName = .x$dist)
        })
      ),
      menuItem(
        i18n()$t("Discrete distributions"),
        icon = icon("bar-chart-o"),
        purrr::map(discrete_distributions, ~ {
          menuSubItem(i18n()$t(.x$name), tabName = .x$dist)
        })
      ),
      menuItem(
        "About", icon = icon("info"),
        tabName = "about"
      ),
      menuItem(
        "Source code", icon = icon("github"),
        href = "http://github.com/ksmzn/ProbabilityDistributionsViewer"
      ),
      tags$li(
        a(
          href = paste0("http://twitter.com/intent/tweet?text=", i18n()$t("Probability Distributions Viewer"), "&url=https://statdist.ksmzn.com/&via=ksmzn&hashtags=rshiny"),
          target = "_blank",
          icon("twitter"),
          onClick = "window.open(encodeURI(decodeURI(this.href)),
            'tweetwindow',
            'width=550, height=450, personalbar=0, toolbar=0, scrollbars=1, resizable=1'
            ); return false;",
          span("Tweet"),
          tags$small(
            class = "badge pull-right bg-light-blue",
            "Share"
          )
        )
      ),
      tags$li(
        a(
          href = paste0("http://www.facebook.com/sharer.php?u=https://statdist.ksmzn.com/&t=", i18n()$t("Probability Distributions Viewer")),
          target = "_blank",
          icon("facebook"),
          span("Facebook"),
          tags$small(
            class = "badge pull-right bg-light-blue",
            "Share"
          )
        )
      ),
      menuItem(
        "@ksmzn", icon = icon("twitter"),
        href = "https://twitter.com/ksmzn"
      ),
      menuItem(
        "Blog", icon = icon("pencil"),
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
