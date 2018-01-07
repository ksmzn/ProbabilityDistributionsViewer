library(shiny)
library(hypergeo)

boxcolor <- "blue"
mean.icon <- icon("star", lib = "glyphicon")
variance.icon <- icon("resize-horizontal", lib = "glyphicon")

# Functions ----
createFormula <- function(f_str, value){
  paste0("\\(", f_str, "\\!=\\!", value, "\\)") 
}

createBox <- function(f_str, value, param_str, param = "mean"){
  if(param == "variance"){
    icon <- variance.icon
  } else {
    icon <- mean.icon
  }
  if(is.null(f_str) | is.null(value)){
    formula <- "定義されない"
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

meanBox <- function(f_str, value, param_str = "期待値"){
  f <- createBox(f_str, value, param_str, param = "mean")
  return(f)
}

varianceBox <- function(f_str, value, param_str = "分散"){
  f <- createBox(f_str, value, param_str, param = "variance")
  return(f)
}

distTab2 <- function(input, output, session, distribution) {
  # Set Up ----
  d <- distribution

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
  if(d$c_or_d == "c"){
    seq_func <- function(min, max) seq(min, max, length=1000)
  } else {
    seq_func <- function(min, max) seq(min, max)
  }

  # Outputs ----
  # Mean
  output$meanBox <- renderValueBox({
    value <- do.call(d$mean, params())
    f_str <- d$mean_str
    meanBox(f_str, value)
  })

  # Variance
  output$varianceBox <- renderValueBox({
    value <- do.call(d$variance, params())
    f_str <- d$variance_str
    varianceBox(f_str, value)
  })

  # Plot
  output$plot <- renderNvd3Chart({
    x <- seq_func(input$range[1], input$range[2])
#     print(c(x = x, params()))
#     x <- do.call(d$x_filter, c(x = x, params()))
    y <- eval(func()(x))
    df <- data.frame(x, y)
    return(df)
  })
}

callDistributionModule <- function(distribution){
  callModule(distTab2, distribution$dist, distribution)
}

server <- function(input, output) {
  ###########################################################################
  # Continuous probability distributions
  ###########################################################################
  callDistributionModule(norm)
  callDistributionModule(erlang)
  callDistributionModule(f)
  callDistributionModule(ncf)
  callDistributionModule(chisq)
  callDistributionModule(ncChisq)
  callDistributionModule(gamma)
  callDistributionModule(cauchy)
  callDistributionModule(exp_dist)
  callDistributionModule(lnormal)
  callDistributionModule(t_dist)
  callDistributionModule(nct)
  callDistributionModule(beta)
  callDistributionModule(ncbeta)
  callDistributionModule(unif)
  callDistributionModule(logis)
  callDistributionModule(weibull)
  ###########################################################################
  # Discrete probability distributions
  ###########################################################################
  callDistributionModule(geom)
  callDistributionModule(hyper)
  callDistributionModule(binom)
  callDistributionModule(nbinom)
  callDistributionModule(pois)
  callDistributionModule(dunif)
}
