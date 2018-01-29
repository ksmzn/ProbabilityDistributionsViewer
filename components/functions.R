library(shiny)
library(shinydashboard)
library(htmltools)

# Variables ----
boxcolor <- "blue"
mean.icon <- icon("star", lib = "glyphicon")
variance.icon <- icon("resize-horizontal", lib = "glyphicon")

# Functions ----

## ui ----
### Custom selectInput
selectLanguageInput <- function (inputId, choices, selected = NULL, selectize = TRUE, width = NULL) {
  selected <- shiny::restoreInput(id = inputId, default = selected)
  choices <- shiny:::choicesWithNames(choices)
  if (is.null(selected)) {
    selected <- shiny:::firstChoice(choices)
  } else{
    selected <- as.character(selected)
  }
  selectTag <- htmltools::tags$select(
    id = inputId,
    shiny:::selectOptions(choices, selected)
  )
  res <- div(
    class = "form-group shiny-input-container",
    style = paste0("width: ", htmltools::validateCssUnit(width), ";"),
    NULL, # For selectizeIt function.
    div(selectTag)
  )
  shiny:::selectizeIt(inputId, res, NULL, nonempty = TRUE)
}

### Panel for Distributions
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

distBox <- function(name, wiki, i18n) {
  box(width = 5,
      status = "primary",
      title = i18n()$t(name),
      paste0(i18n()$t("Reference"), " : "),
      a(target = "_blank",
        href = i18n()$t(wiki),
        'Wikipedia',
        img(src = 'img/external.png')
      )
  )
}

### Formula Box
formulaBox <- function(f_str, c_or_d, i18n){
  if(c_or_d == "c"){
    f_title <- "Probability density function (PDF)" 
  } else {
    f_title <- "Probability mass function" 
  }
  f_text <- paste0("$$", f_str, "$$")
  box(
    width = 7,
    status = "primary",
    title = i18n()$t(f_title),
    helpText(f_text)
  )
}

### Sliders
createSlider <- function(name, label, min, max, value, step = 1L){
  sliderInput(
    inputId = name,
    label = label,
    min = min, max = max, value = value, step = step
  )
}
### Parameters Box
createParamBox <- function(ns, c_or_d, rangeArgs, paramArgs = NULL, p_or_c = NULL, i18n = NULL){
  # Selector
  choices <- c("p", "c")
  if(c_or_d == "c") {
    pdf <- i18n()$t("Probability density function (PDF)")
  } else {
    pdf <- i18n()$t("Probability mass function (PMF)")
  }
  cdf <- i18n()$t("Cumulative distribution function (CDF)")
  names(choices) <- c(pdf, cdf)
  pcButton <- radioButtons(ns("p_or_c"), "", choices, p_or_c)
  
  # Range Slider
  rangeArgs$name <- ns("range")
  rangeArgs$label <- i18n()$t("Range")
  rangeSlider <- do.call(createSlider, rangeArgs)

  # Parameter Sliders
  if(is.null(paramArgs)){
    paramSliders <- NULL
  } else {
    paramSliders <- lapply(paramArgs, function(x){
      x$name <- ns(x$name)
      label_name <- x$label_name
      label_symbol <- paste0("\\(", x$label_symbol, "\\)")
      if(is.na(label_name) || label_name == ''){
        x$label <- label_symbol
      } else {
        label_name <- i18n()$t(label_name)
        x$label <- paste(label_name, label_symbol)
      }
      # Remove "label_name" and "label_symbol"
      x <- x[!(names(x) %in% c("label_name", "label_symbol"))]
      do.call(createSlider, x)
    })
  }
  
  # Box
  paramBox <- 
    do.call(
      box,
      list(
        width = 5,
        title = i18n()$t("Parameters"),
        status = "primary",
        solidHeader = TRUE,
        withMathJax(),
        pcButton,
        rangeSlider,
        paramSliders
      )
    )
  return(paramBox)
}

### Dynamic Value Box
valueBoxRow <- function(ns, width = 6L){
  fluidRow(
    valueBoxOutput(ns("meanBox"), width = width),
    valueBoxOutput(ns("varianceBox"), width = width)
  )
}
valueBoxRowWide <- function(ns){
  valueBoxRow(ns, width = 12L)
}

## server ----
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

