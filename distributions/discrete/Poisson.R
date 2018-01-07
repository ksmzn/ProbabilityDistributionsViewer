# ポアソン分布 ----
## Functions ----
pois.func_p <- function(lambda) function(x) dpois(x, lambda=lambda)
pois.func_c <- function(lambda) function(x) ppois(x, lambda=lambda)
pois.formula <- "
P(X=k)=\\frac{\\lambda^k e^{-\\lambda}}{k!}
"

pois.x_filter <- NULL

## Moments ----
pois.mean <- function(lambda) lambda
pois.mean_str <- "\\lambda"
pois.variance <- function(lambda) lambda
pois.variance_str <- "\\lambda"

## Parameters ----
pois.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 1
)
pois.lambda <- list(
  name = "lambda",
  label = "\\(\\lambda\\)",
  min = 1,
  max = 20,
  value = 1,
  step = 0.5
)

## Instance ----
pois <- Distribution$new(
  dist = "pois",
  name = "Poisson distribution",
  wiki = "https://en.wikipedia.org/wiki/Poisson_distribution",
  c_or_d = "d",
  formula = pois.formula,
  func_p = pois.func_p,
  func_c = pois.func_c,
  x_filter = pois.x_filter,
  mean = pois.mean,
  mean_str = pois.mean_str,
  variance = pois.variance,
  variance_str = pois.variance_str,
  range = pois.range,
  pois.lambda
)
