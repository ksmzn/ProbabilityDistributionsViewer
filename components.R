library(shiny)
library(shinydashboard)
####################################################
# Functions ----
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

# Parameter Box
createSlider <- function(dist, name, label, min, max, value, step = 1L){
  sliderInput(
    inputId = paste(dist, name, sep="."),
    label = label,
    min = min, max = max, value = value, step = step
  )
}
createRangeSlider <- function(dist, min, max, value, step = 1L){
  createSlider(
    dist = dist,
    name = "range",
    label = "範囲",
    min = min, max = max, value = value, step = step
  )
}
createParamBox <- function(dist, rangeArgs, paramArgs = NULL){
  pcButton <- radioButtons(paste(dist, "p_or_c", sep="."), "", c("確率密度関数"="p", "累積分布関数"="c"))
  
  # Range Slider
  rangeSlider <- do.call(createRangeSlider, rangeArgs)
  if(is.null(paramArgs)){
    paramSliders <- NULL
  } else {
    # Parameter Sliders
    paramSliders <- lapply(paramArgs, function(x){do.call(createSlider, x)})
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
plotBox <- function(dist, c_or_d = "c"){
  if(c_or_d == "c"){
    type <- "line"
  } else {
    type <- "scatter"
  }
  box(
    width = 7,
    title = "プロット", status = "primary", solidHeader = TRUE,
    nvd3ChartOutput(paste0(dist, "Plot"), type)
  )
}

# Dynamic Value Box
valueBoxRow <- function(dist, width = 6L){
  fluidRow(
    valueBoxOutput(paste(dist, "meanBox", sep = "."), width = width),
    valueBoxOutput(paste(dist, "varianceBox", sep = "."), width = width)
  )
}
valueBoxRowWide <- function(dist){
  valueBoxRow(dist, width = 12L)
}

distTab <- function(distEnv){
  dist <- distEnv[["dist"]]
  c_or_d <- distEnv[["c_or_d"]]
  item <- tabItem(tabName = dist,
    fluidRow(
      distBox(distEnv[["name"]], distEnv[["wiki"]]),
      formulaBox(distEnv[["formula"]], c_or_d)
    ),
    fluidRow(
      createParamBox(dist = dist, distEnv[["range"]], distEnv[["params"]]),
      plotBox(dist, c_or_d)
    ),
    valueBoxRow(dist)
  )
  return(item)
}


