# ガンマ分布 ----
## Functions ----
gamma.func_p <- function(shape, scale) function(x) dgamma(x, shape=shape, scale=scale)
gamma.func_c <- function(shape, scale) function(x) pgamma(x, shape=shape, scale=scale)
gamma.formula <- "f(x) = x^{k-1} \\frac{e^{-x/\\theta}}{\\Gamma(k)\\,\\theta^k}
        \\ \\ \\ \\ \\mathrm{for\\ } x > 0
"

gamma.x_filter <- function(x, shape, scale) {
  if (shape < 1) {
    x <- x[x!=0]
  }
  return(x)
}

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
  label_name = "Shape",
  label_symbol = "k",
  min = 0,
  max = 20,
  value = 1,
  step = 0.1
)
gamma.scale <- list(
  name = "scale",
  label_name = "Scale",
  label_symbol = "\\theta",
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
  func_p = gamma.func_p,
  func_c = gamma.func_c,
  formula = gamma.formula,
  x_filter = gamma.x_filter,
  mean = gamma.mean,
  mean_str = gamma.mean_str,
  variance = gamma.variance,
  variance_str = gamma.variance_str,
  range = gamma.range,
  gamma.shape,
  gamma.scale
)
