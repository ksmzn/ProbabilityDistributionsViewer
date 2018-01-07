# 正規分布 ----
## Functions ----
norm.func_p <- function(mean, sd) function(x) dnorm(x, mean=mean, sd=sd)
norm.func_c <- function(mean, sd) function(x) pnorm(x, mean=mean, sd=sd)
norm.formula <- "f(x)=\\frac{1}{\\sqrt{2\\pi\\sigma^{2}}}\\exp\\!\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2} \\right)"

norm.x_filter <- NULL

## Moments ----
norm.mean <- function(mean, sd) mean
norm.mean_str <- "\\mu"
norm.variance <- function(mean, sd) sd ** 2
norm.variance_str <- "\\sigma^2"

## Parameters ----
norm.range <- list(
  min = -100,
  max = 100,
  value = c(-10, 10)
)
norm.meanParam <- list(
  name = "mean",
  label = "平均 \\(\\mu\\)",
  min = -50,
  max = 50,
  value = 0
)
norm.sd <- list(
  name = "sd",
  label = "標準偏差 \\(\\sigma\\)",
  min = 0,
  max = 10,
  value = 1,
  step = 0.5
)


## Instance ----
norm <- Distribution$new(
  dist = "norm",
  name = "Normal distribution",
  wiki = "https://en.wikipedia.org/wiki/Normal_distribution",
  c_or_d = "c",
  formula = norm.formula,
  func_p = norm.func_p,
  func_c = norm.func_c,
  x_filter = norm.x_filter,
  mean = norm.mean,
  mean_str = norm.mean_str,
  variance = norm.variance,
  variance_str = norm.variance_str,
  range = norm.range,
  norm.meanParam,
  norm.sd
)
