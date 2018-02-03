library(shiny)
library(shinydashboard)

# Functions
source("components/functions.R")

# Module ----
distTabUI <- function(distEnv, wide = F) {
  if (wide) {
    boxRow <- valueBoxRowWide
  } else {
    boxRow <- valueBoxRow
  }
  dist <- distEnv[["dist"]]
  c_or_d <- distEnv[["c_or_d"]]
  ns <- NS(dist)

  item <- tabItem(
    tabName = dist,
    fluidRow(
      uiOutput(ns("distBox")),
      uiOutput(ns("formulaBox"))
    ),
    fluidRow(
      uiOutput(ns("paramBox")),
      uiOutput(ns("plotBox"))
    ),
    boxRow(ns)
  )
  return(item)
}

distTab <- function(input, output, session, distribution, initParams, i18n) {
  # Set Up ----
  d <- distribution
  c_or_d <- d$c_or_d

  # Parameters
  param_names <- names(d$params)
  params <- reactive({
    li <- lapply(param_names, function(x) {
      # `input` is not allowed bracket indexing.
      eval(parse(text = paste0("input$", x)))
    })
    names(li) <- param_names
    return(li)
  })

  # Function
  func_base <- reactive(d$func(input$p_or_c))
  func <- reactive(do.call(func_base(), params()))

  # X axis
  if (c_or_d == "c") {
    seq_func <- function(min, max) seq(min, max, length = 1000)
    plot_type <- "line"
  } else {
    seq_func <- function(min, max) seq(min, max)
    plot_type <- "scatter"
  }

  # Outputs ----
  # Distribution Box
  output$distBox <- renderUI({
    distBox(d$name, d$wiki, i18n)
  })

  # Formula Box
  output$formulaBox <- renderUI({
    formulaBox(d$formula, c_or_d, i18n)
  })

  # Parameter Box
  output$paramBox <- renderUI({
    ns <- session$ns

    range_list <- d$range
    params_list <- d$params
    dist_name <- d$dist

    # Handle initial values when language changes
    params_kept <- initParams()
    new_params <- params_kept[[dist_name]] # parameters before language changes

    if (is.null(new_params)) {
      p_or_c <- NULL
    } else {
      # Set as default values
      p_or_c <- new_params$p_or_c
      range_list$value <- new_params$range
      for (x in param_names) {
        params_list[[x]]$value <- new_params[[x]]
      }
    }
    createParamBox(ns, c_or_d, range_list, params_list, p_or_c, i18n)
  })

  # Mean
  output$meanBox <- renderValueBox({
    value <- do.call(d$mean, params())
    f_str <- d$mean_str
    meanBox(f_str, value, i18n)
  })

  # Variance
  output$varianceBox <- renderValueBox({
    value <- do.call(d$variance, params())
    f_str <- d$variance_str
    varianceBox(f_str, value, i18n)
  })

  # Plot
  output$plot <- renderNvd3Chart({
    x <- seq_func(input$range[1], input$range[2])
    x <- do.call(d$x_filter, append(list(x = x), params()))
    y <- eval(func()(x))
    df <- data.frame(x, y)
    return(df)
  })

  # Plot Box
  output$plotBox <- renderUI({
    ns <- session$ns
    box(
      width = 7,
      title = i18n()$t("Plot"), status = "primary", solidHeader = TRUE,
      nvd3ChartOutput(ns("plot"), plot_type)
    )
  })
}

callDistributionModule <- function(distribution, initParams, i18n = NULL) {
  callModule(distTab, distribution$dist, distribution, initParams, i18n)
}
