# 指数分布 ----
## Functions ----
exp_dist.func_p <- function(rate) function(x) dexp(x, rate=rate)
exp_dist.func_c <- function(rate) function(x) pexp(x, rate=rate)
exp_dist.formula <- "
f(x; \\lambda) = \\left\\{
  \\begin{array}{ll}
  \\lambda e^{-\\lambda x} & (x \\geq 0) \\\\ 0 & (x < 0)
  \\end{array}
  \\right.
"

## Moments ----
exp_dist.mean <- function(rate) 1 / rate
exp_dist.mean_str <- "\\frac{1}{\\lambda}"
exp_dist.variance <- function(rate) 1 / (rate ** 2)
exp_dist.variance_str <- "\\frac{1}{\\lambda^2}"

## Parameters ----
exp_dist.range <- list(
  min = -10,
  max = 50,
  value = c(0, 5),
  step= 1
)
exp_dist.rate <- list(
  name = "rate",
  label = "\\(\\lambda\\)",
  min = 0,
  max = 20,
  value = 1,
  step = 0.1
)

## Instance ----
exp_dist <- Distribution$new(
  dist = "exp_dist",
  name = "Exponential distribution",
  wiki = "https://en.wikipedia.org/wiki/Exponential_distribution",
  c_or_d = "c",
  formula = exp_dist.formula,
  func_p = exp_dist.func_p,
  func_c = exp_dist.func_c,
  mean = exp_dist.mean,
  mean_str = exp_dist.mean_str,
  variance = exp_dist.variance,
  variance_str = exp_dist.variance_str,
  range = exp_dist.range,
  exp_dist.rate
)
