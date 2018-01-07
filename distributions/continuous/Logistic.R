# ロジスティック分布 ----
## Functions ----
logis.func_p <- function(location, scale) function(x) dlogis(x, location=location, scale=scale)
logis.func_c <- function(location, scale) function(x) plogis(x, location=location, scale=scale)
logis.formula <- "
f(x;\\mu,s) = \\frac{\\exp(-\\frac{x-\\mu}{s})}{s(1+\\exp(-\\frac{x-\\mu}{s}))^2}
"

## Moments ----
logis.mean <- function(location, scale) location
logis.mean_str <- "\\mu"
logis.variance <- function(location, scale) ((scale * pi) ** 2) / 3
logis.variance_str <-  "\\frac{\\pi^2}{3} s^2"

## Parameters ----
logis.range <- list(
  min = -100,
  max = 100,
  value = c(-10, 10),
  step= 0.5
)
logis.location <- list(
  name = "location",
  label = "位置 \\(\\mu\\)",
  min = -20,
  max = 20,
  value = 0,
  step= 0.1
)
logis.scale <- list(
  name = "scale",
  label = "尺度 \\(s\\)",
  min = 0,
  max = 20,
  value = 1,
  step = 0.1
)

## Instance ----
logis <- Distribution$new(
  dist = "logis",
  name = "Logistic distribution",
  wiki = "https://en.wikipedia.org/wiki/Logistic_distribution",
  c_or_d = "c",
  formula = logis.formula,
  func_p = logis.func_p,
  func_c = logis.func_c,
  mean = logis.mean,
  mean_str = logis.mean_str,
  variance = logis.variance,
  variance_str = logis.variance_str,
  range = logis.range,
  logis.location,
  logis.scale
)
