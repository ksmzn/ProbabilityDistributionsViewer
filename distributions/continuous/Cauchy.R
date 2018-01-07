# コーシー分布
## Functions ----
cauchy.func_p <- function(location, scale) function(x) dcauchy(x, location=location, scale=scale)
cauchy.func_c <- function(location, scale) function(x) pcauchy(x, location=location, scale=scale)
cauchy.formula <- "
\\begin{align}
  f(x; x_0,\\gamma) &=
  { 1 \\over \\pi } \\left[ { \\gamma \\over (x - x_0)^2 + \\gamma^2  } \\right]
\\end{align}
"

cauchy.x_filter <- NULL

## Moments ----
cauchy.mean <- function(...) NULL
cauchy.mean_str <- NULL
cauchy.variance <- function(...) NULL
cauchy.variance_str <- NULL

## Parameters ----
cauchy.range <- list(
  min = -100,
  max = 100,
  value = c(-10, 10),
  step= 0.5
)
cauchy.location <- list(
  name = "location",
  label = "位置 \\(x_0\\)",
  min = -20,
  max = 20,
  value = 0,
  step= 0.1
)
cauchy.scale <- list(
  name = "scale",
  label = "尺度 \\(\\gamma\\)",
  min = 0,
  max = 20,
  value = 1,
  step = 0.1
)

## Instance ----
cauchy <- Distribution$new(
  dist = "cauchy",
  name = "Cauchy distribution",
  wiki = "https://en.wikipedia.org/wiki/Cauchy_distribution",
  c_or_d = "c",
  formula = cauchy.formula,
  func_p = cauchy.func_p,
  func_c = cauchy.func_c,
  x_filter = cauchy.x_filter,
  mean = cauchy.mean,
  mean_str = cauchy.mean_str,
  variance = cauchy.variance,
  variance_str = cauchy.variance_str,
  range = cauchy.range,
  cauchy.location,
  cauchy.scale
)
