library(shiny)
library(shinydashboard)
# Functions ----
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
# plotBox <- function(id, c_or_d = "c"){
#   uiOutput(id)
#   if(c_or_d == "c"){
#     type <- "line"
#   } else {
#     type <- "scatter"
#   }
#   box(
#     width = 7,
#     title = "プロット", status = "primary", solidHeader = TRUE,
#     nvd3ChartOutput(id, type)
#   )
# }

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
#       plotBox(ns("plot"), c_or_d)
      uiOutput(ns("plotBox"))
    ),
    boxRow(ns)
  )
  return(item)
}
