# 離散一様分布
## functions ----
dunif.func_p <- function(min, max) function(x) dunif(x, min=min, max=max)
dunif.func_c <- function(min, max) function(x) punif(x, min=min, max=max)
dunif.formula <- "
f(x)=\\begin{cases}
\\frac{1}{n} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
\\end{cases} 
"

dunif.x_filter <- NULL

## Moments ----
dunif.mean <- function(min, max) (min + max) / 2
dunif.mean_str <- "\\frac{a+b}{2}"
dunif.variance <- function(min, max) (((max - min + 1) ** 2) - 1) / 12
dunif.variance_str <- "\\frac{(b-a+1)^2 -1}{12}"

## Parameters ----
dunif.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 1
)
dunif.min <- list(
  name = "min",
  label_name = "Min",
  label_symbol = "a",
  min = 0,
  max = 100,
  value = 0,
  step = 1
)
dunif.max <- list(
  name = "max",
  label_name = "Max",
  label_symbol = "b",
  min = 0,
  max = 100,
  value = 20,
  step = 1
)

## Instance ----
dunif <- Distribution$new(
  dist = "dunif",
  name = "Discrete uniform distribution",
  wiki = "https://en.wikipedia.org/wiki/Discrete_uniform_distribution",
  c_or_d = "d",
  formula = dunif.formula,
  func_p = dunif.func_p,
  func_c = dunif.func_c,
  x_filter = dunif.x_filter,
  mean = dunif.mean,
  mean_str = dunif.mean_str,
  variance = dunif.variance,
  variance_str = dunif.variance_str,
  range = dunif.range,
  dunif.min,
  dunif.max
)
