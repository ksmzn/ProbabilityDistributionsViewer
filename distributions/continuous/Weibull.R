# ワイブル分布 ----
## Functions ----
weibull.func_p <- function(shape, scale) function(x) dweibull(x, shape=shape, scale=scale)
weibull.func_c <- function(shape, scale) function(x) pweibull(x, shape=shape, scale=scale)
weibull.formula <- "
f(t)=\\frac{m}{\\eta}\\left(\\frac{t}{\\eta}\\right)^{m-1}
\\exp \\left\\{-\\left(\\frac{t}{\\eta}\\right)^m\\right\\}
"

## Moments ----
weibull.mean <- function(shape, scale) scale * gamma(1 + 1 / shape)
weibull.mean_str <- "\\eta \\Gamma(1+1/m)"
weibull.variance <- function(shape, scale) {
  (scale ** 2) * (gamma(1 + 2 / shape) - gamma(1 + 1 / shape) ** 2)
}
weibull.variance_str <- "\\eta^2\\left[\\Gamma\\left(1+\\frac{2}{m}\\right) - \\left(\\Gamma\\left(1+\\frac{1}{m}\\right)\\right)^2\\right]"

## Parameters ----
weibull.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 0.5
)
weibull.shape <- list(
  name = "shape",
  label = "形状 \\(m\\)",
  min = 0,
  max = 20,
  value = 1,
  step = 0.1
)
weibull.scale <- list(
  name = "scale",
  label = "尺度 \\(\\eta\\)",
  min = 0,
  max = 20,
  value = 1,
  step = 0.1
)

## Instance ----
weibull <- Distribution$new(
  dist = "weibull",
  name = "Weibull distribution",
  wiki = "https://en.wikipedia.org/wiki/Weibull_distribution",
  c_or_d = "c",
  formula = weibull.formula,
  func_p = weibull.func_p,
  func_c = weibull.func_c,
  mean = weibull.mean,
  mean_str = weibull.mean_str,
  variance = weibull.variance,
  variance_str = weibull.variance_str,
  range = weibull.range,
  weibull.shape,
  weibull.scale
)
