# 対数正規分布 ----
## Functions ----
lnormal.func_p <- function(meanlog, sdlog) function(x) dlnorm(x, meanlog=meanlog, sdlog=sdlog)
lnormal.func_c <- function(meanlog, sdlog) function(x) plnorm(x, meanlog=meanlog, sdlog=sdlog)
lnormal.formula <- "
f(x) = \\frac{1}{\\sqrt{2\\pi} \\sigma x} e^{-\\frac{ (\\ln{x}-\\mu)^2}{2\\sigma^2} },
  \\quad 0<x< \\infty
"

## Moments ----
lnormal.mean <- function(meanlog, sdlog) exp(meanlog + ((sdlog ** 2) / 2))
lnormal.mean_str <- "e^{\\mu+\\sigma^2/2}"
lnormal.variance <- function(meanlog, sdlog) exp(2 * meanlog + sdlog) * (exp(sdlog ** 2) - 1)
lnormal.variance_str <- "e^{2\\mu+\\sigma^2}(e^{\\sigma^2}-1)"

## Parameters ----
lnormal.range <- list(
  min = 0,
  max = 200,
  value = c(0, 20),
  step= 0.5
)
lnormal.meanlog <- list(
  name = "meanlog",
  label = "平均log",
  min = -30,
  max = 30,
  value = 0,
  step= 0.05
)
lnormal.sdlog <- list(
  name = "sdlog",
  label = "標準偏差log",
  min = 0,
  max = 10,
  value = 1,
  step = 0.05
)


## Instance ----
lnormal <- Distribution$new(
  dist = "lnormal",
  name = "Log-normal distribution",
  wiki = "https://en.wikipedia.org/wiki/Log-normal_distribution",
  c_or_d = "c",
  formula = lnormal.formula,
  func_p = lnormal.func_p,
  func_c = lnormal.func_c,
  mean = lnormal.mean,
  mean_str = lnormal.mean_str,
  variance = lnormal.variance,
  variance_str = lnormal.variance_str,
  range = lnormal.range,
  lnormal.meanlog,
  lnormal.sdlog
)
