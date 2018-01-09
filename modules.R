library(shiny)
library(shinydashboard)
####################################################
# UI ----
####################################################
# Panel for Distributions
distPanel <- function(name, en) {
  if (missing(en)) {
    wiki = paste0('http://ja.wikipedia.org/wiki/', name)
  } else {
    wiki = paste0('http://en.wikipedia.org/wiki/', en)
  }
  box(width = 5,
      status = "primary",
      title = name,
      "参考 : ",
    a(target = "_blank",
      href = wiki,
      'Wikipedia',
      img(src = 'img/external.png')
    )
  )
}

distBox <- function(name, wiki) {
  box(width = 5,
      status = "primary",
      title = name,
      "参考 : ",
      a(target = "_blank",
        href = wiki,
        'Wikipedia',
        img(src = 'img/external.png')
      )
  )
}

# Formula Box
formulaBox <- function(f_str, c_or_d){
  if(c_or_d == "c"){
    f_title <- "確率密度関数" 
  } else {
    f_title <- "確率関数" 
  }
  f_text <- paste0("$$", f_str, "$$")
  box(
    width = 7,
    status = "primary",
    title = f_title, 
    helpText(f_text)
  )
}

# Parameter Input UI
## Sliders
createSlider <- function(name, label, min, max, value, step = 1L){
  sliderInput(
    inputId = name,
    label = label,
    min = min, max = max, value = value, step = step
  )
}
createRangeSlider <- function(name, min, max, value, step = 1L){
  createSlider(
    name = name,
    label = "範囲",
    min = min, max = max, value = value, step = step
  )
}
createParamBox <- function(ns, rangeArgs, paramArgs = NULL){
  pcButton <- radioButtons(ns("p_or_c"), "", c("確率密度関数"="p", "累積分布関数"="c"))
  
  # Range Slider
  rangeArgs$name <- ns("range")
  rangeSlider <- do.call(createRangeSlider, rangeArgs)
  if(is.null(paramArgs)){
    paramSliders <- NULL
  } else {
    # Parameter Sliders
    paramSliders <- lapply(paramArgs, function(x){
      x$name <- ns(x$name)
      do.call(createSlider, x)
    })
  }
  
  # Box
  paramBox <- 
    do.call(
      box,
      list(
        width = 5, title = "パラメータ", status = "primary", solidHeader = TRUE,
        withMathJax(),
        pcButton,
        rangeSlider,
        paramSliders
      )
    )
  return(paramBox)
}


# Plot Box
plotBox <- function(id, c_or_d = "c"){
  if(c_or_d == "c"){
    type <- "line"
  } else {
    type <- "scatter"
  }
  box(
    width = 7,
    title = "プロット", status = "primary", solidHeader = TRUE,
    nvd3ChartOutput(id, type)
  )
}

# Dynamic Value Box
valueBoxRow <- function(ns, width = 6L){
  fluidRow(
    valueBoxOutput(ns("meanBox"), width = width),
    valueBoxOutput(ns("varianceBox"), width = width)
  )
}
valueBoxRowWide <- function(ns){
  valueBoxRow(ns, width = 12L)
}

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
      distBox(distEnv[["name"]], distEnv[["wiki"]]),
      formulaBox(distEnv[["formula"]], c_or_d)
    ),
    fluidRow(
      createParamBox(ns, distEnv[["range"]], distEnv[["params"]]),
      plotBox2(ns("plot"), c_or_d)
    ),
    boxRow(ns)
  )
  return(item)
}

####################################################
# Server ----
####################################################
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

# Module ----
distTab <- function(input, output, session, distribution) {
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
    x <- do.call(d$x_filter, append(list(x = x), params()))
    y <- eval(func()(x))
    df <- data.frame(x, y)
    return(df)
  })
}

callDistributionModule <- function(distribution){
  callModule(distTab, distribution$dist, distribution)
}

