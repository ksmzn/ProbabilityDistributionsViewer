library(shiny)
library(shinydashboard)
# Variables ----
boxcolor <- "blue"
mean.icon <- icon("star", lib = "glyphicon")
variance.icon <- icon("resize-horizontal", lib = "glyphicon")

# Functions ----
createFormula <- function(f_str, value){
  paste0("\\(", f_str, "\\!=\\!", value, "\\)") 
}

createBox <- function(f_str, value, param = "Mean", i18n = NULL){
  if(param == "Variance"){
    icon <- variance.icon
  } else {
    icon <- mean.icon
  }
  param_str <- i18n()$t(param)
  if(is.null(f_str) | is.null(value)){
    formula <- i18n()$t("Undefined")
  } else {
    value <- round(value, digits = 3)
    formula <- createFormula(f_str, value)
    formula <- withMathJax(formula)
  }
  box <-
    valueBox(
      formula,
      param_str,
      icon = icon,
      color = boxcolor
    )
  return(box)
}

meanBox <- function(f_str, value, i18n){
  f <- createBox(f_str, value, param = "Mean", i18n)
  return(f)
}

varianceBox <- function(f_str, value, i18n){
  f <- createBox(f_str, value, param = "Variance", i18n)
  return(f)
}

# Module ----
distTab <- function(input, output, session, distribution, i18n) {
  # Set Up ----
  d <- distribution
  is_continious <- d$c_or_d == "c"

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
  if(is_continious){
    seq_func <- function(min, max) seq(min, max, length=1000)
    plot_type <- "line"
  } else {
    seq_func <- function(min, max) seq(min, max)
    plot_type <- "scatter"
  }

  # Outputs ----
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

