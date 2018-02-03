library(shiny)

# To be called from ui.R
nvd3ChartOutput <- function(inputId, type, width="100%", height="400px") {
  style <- sprintf(
    "width: %s; height: %s;",
    validateCssUnit(width), validateCssUnit(height)
  )
  if (type == "line") {
    class <- "nvd3-linechart"
  } else {
    class <- "nvd3-scatterchart"
  }

  tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple lineChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    singleton(tags$head(
      tags$script(src = "js/d3/d3.v3.min.js"),
      tags$script(src = "js/nvd3/nv.d3.min.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "js/nvd3/nv.d3.min.css"),
      tags$script(src = "js/linechart-binding.js"),
      tags$script(src = "js/scatterchart-binding.js")
    )),
    div(
      id = inputId, class = class, style = style,
      tag("svg", list())
    )
  )
}

# To be called from server.R
renderNvd3Chart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
  installExprFunction(expr, "func", env, quoted)

  function() {
    dataframe <- func()
    mapply(function(name) {
      values <- mapply(function(i, j) {
        list(x = i, y = j)
      }, dataframe$x, dataframe$y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
      list(key = name, values = values)
    }, c("y"), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
}


# Data frame or list looks like:
#
# {
#   "Series A": [1,2,3,4,5],
#   "Series B": [6,7,8,9,10]
# }
#
# D3 expects:
#
# [
#   {
#     key: "Series A",
#     values: [{x:1,y:1}, {x:2,y:2}, {x:3,y:3}, {x:4,y:4}, {x:5,y:5}]
#   },
#   {
#     key: "Series B",
#     values: [{x:1,y:6}, {x:2,y:7}, {x:3,y:8}, {x:4,y:9}, {x:5,y:10}]
#   }
# ]