library(shiny)
library(shinydashboard)

# Functions
source("components/functions.R")

# Module ----
distTabUI <- function(distEnv, wide = F){
  if(wide){
    boxRow <- valueBoxRowWide
  } else {
    boxRow <- valueBoxRow
  }
  dist <- distEnv[["dist"]]
  c_or_d <- distEnv[["c_or_d"]]
  ns <- NS(dist)

  item <- tabItem(tabName = dist,
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

distTab <- function(input, output, session, distribution, i18n) {
  # Set Up ----
  d <- distribution
  c_or_d <- d$c_or_d

  # Parameters
  param_names <- names(d$params)
  params <- reactive({
    li <- lapply(param_names, function(x){
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
  if(c_or_d == "c"){
    seq_func <- function(min, max) seq(min, max, length=1000)
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
    targets <- c(c("p_or_c", "range"), param_names)

    # Update initial values
    if(is.null(input$p_or_c)){
      p_or_c <- NULL
    } else {
      for(x in targets){
        res <- input[[x]]
        if(x == "p_or_c"){
          p_or_c <- res
        } else  if(x == "range"){
          d$range$value <- res
        } else if(x %in% param_names) {
          d$params[[x]]$value <- res
        }
      }
    }
    createParamBox(ns, c_or_d, d$range, d$params, p_or_c, i18n)
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

callDistributionModule <- function(distribution, i18n = NULL){
  callModule(distTab, distribution$dist, distribution, i18n)
}

