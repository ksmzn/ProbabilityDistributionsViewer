# ガンマ分布 ----
## Functions ----
gamma.func_p <- function(shape, scale) function(x) dgamma(x, shape=shape, scale=scale)
gamma.func_c <- function(shape, scale) function(x) pgamma(x, shape=shape, scale=scale)
gamma.formula <- "f(x) = x^{k-1} \\frac{e^{-x/\\theta}}{\\Gamma(k)\\,\\theta^k}
        \\ \\ \\ \\ \\mathrm{for\\ } x > 0
"

## Moments ----
gamma.mean <- function(shape, scale) shape * scale
gamma.mean_str <- "k\\theta"
gamma.variance <- function(shape, scale) shape * (scale ** 2)
gamma.variance_str <- "k\\theta^2"

## Parameters ----
gamma.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 0.5
)
gamma.shape <- list(
  name = "shape",
  label = "形状 \\(k\\)",
  min = 0,
  max = 20,
  value = 1,
  step = 0.1
)
gamma.scale <- list(
  name = "scale",
  label = "尺度 \\(\\theta\\)",
  min = 0,
  max = 20,
  value = 1,
  step = 0.1
)

## Instance ----
gamma <- Distribution$new(
  dist = "gamma",
  name = "Gamma distribution",
  wiki = "https://en.wikipedia.org/wiki/Gamma_distribution",
  c_or_d = "c",
  formula = gamma.formula,
  func_p = gamma.func_p,
  func_c = gamma.func_c,
  mean = gamma.mean,
  mean_str = gamma.mean_str,
  variance = gamma.variance,
  variance_str = gamma.variance_str,
  range = gamma.range,
  gamma.shape,
  gamma.scale
)
