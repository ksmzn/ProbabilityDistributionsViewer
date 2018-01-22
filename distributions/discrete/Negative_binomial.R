# 負の二項分布 ----
## Functions ----
nbinom.func_p <- function(size, prob) function(x) dnbinom(x, size=size, prob=prob)
nbinom.func_c <- function(size, prob) function(x) pnbinom(x, size=size, prob=prob)
nbinom.formula <- "f(x)=P(X=x) = {x-1 \\choose r-1} p^r (1-p)^{x-r}"

nbinom.x_filter <- NULL

## Moments ----
nbinom.mean <- function(size, prob){
  if(is.null(prob) || prob == 0){
    value <- Inf
  } else {
    value <- size / prob
  }
  return(value)
}
nbinom.mean_str <- "\\frac{r}{p}"
nbinom.variance <- function(size, prob){
  if(is.null(prob) || prob == 0){
    value <- Inf
  } else {
    value <- size * (1 - prob) / (prob ** 2) 
  }
  return(value)
}
nbinom.variance_str <- "\\frac{r(1-p)}{p^2}"

## Parameters ----
nbinom.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 1
)
nbinom.size <- list(
  name = "size",
  label_name = "target for number of successful trials",
  label_symbol = "r",
  min = 1,
  max = 20,
  value = 1,
  step = 1
)
nbinom.prob <- list(
  name = "prob",
  label_name = "Probability of successful trial",
  label_symbol = "p",
  min = 0,
  max = 1,
  value = 0.5,
  step = 0.01
)

## Instance ----
nbinom <- Distribution$new(
  dist = "nbinom",
  name = "Negative binomial distribution",
  wiki = "https://en.wikipedia.org/wiki/Negative_binomial_distribution",
  c_or_d = "d",
  formula = nbinom.formula,
  func_p = nbinom.func_p,
  func_c = nbinom.func_c,
  x_filter = nbinom.x_filter,
  mean = nbinom.mean,
  mean_str = nbinom.mean_str,
  variance = nbinom.variance,
  variance_str = nbinom.variance_str,
  range = nbinom.range,
  nbinom.size,
  nbinom.prob
)
