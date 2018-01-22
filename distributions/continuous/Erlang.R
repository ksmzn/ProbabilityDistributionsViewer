# アーラン分布 ----
## Functions ----
erlang.func_p <- function(shape, scale) function(x) dgamma(x, shape=shape, scale=scale)
erlang.func_c <- function(shape, scale) function(x) pgamma(x, shape=shape, scale=scale)
erlang.formula <- "f(x; k, \\theta)=\\frac{x^{k-1}e^{-x/\\theta}}{\\theta^{k}(k-1)!} \\quad\\mbox{for }x,\\theta > 0"

erlang.x_filter <- NULL

## Moments ----
erlang.mean <- function(shape, scale) shape * scale
erlang.mean_str <- "k\\theta"
erlang.variance <- function(shape, scale) shape * (scale ** 2)
erlang.variance_str <- "k\\theta^2"

## Parameters ----
erlang.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 0.5
)
erlang.shape <- list(
  name = "shape",
  label_name = "Shape",
  label_symbol = "k",
  min = 1,
  max = 20,
  value = 1
)
erlang.scale <- list(
  name = "scale",
  label_name = "Scale",
  label_symbol = "\\theta",
  min = 0.5,
  max = 20,
  value = 1,
  step = 0.1
)

## Instance ----
erlang <- Distribution$new(
  dist = "erlang",
  name = "Erlang distribution",
  wiki = "https://en.wikipedia.org/wiki/Erlang_distribution",
  c_or_d = "c",
  func_p = erlang.func_p,
  func_c = erlang.func_c,
  formula = erlang.formula,
  x_filter = erlang.x_filter,
  mean = erlang.mean,
  mean_str = erlang.mean_str,
  variance = erlang.variance,
  variance_str = erlang.variance_str,
  range = erlang.range,
  erlang.shape,
  erlang.scale
)
