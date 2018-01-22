# 二項分布 ----
## Functions ----
binom.func_p <- function(size, prob) function(x) dbinom(x, size=size, prob=prob)
binom.func_c <- function(size, prob) function(x) pbinom(x, size=size, prob=prob)
binom.formula <- "
P[X=k]={n\\choose k}p^k(1-p)^{n-k}\\quad\\mbox{for}\\ k=0,1,2,\\dots,n
"

binom.x_filter <- NULL

## Moments ----
binom.mean <- function(size, prob) size * prob
binom.mean_str <- "np"
binom.variance <- function(size, prob) size * prob * (1 - prob)
binom.variance_str <- "np(1-p)"

## Parameters ----
binom.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 1
)
binom.size <- list(
  name = "size",
  label_name = "Number of trials",
  label_symbol = "n",
  min = 0,
  max = 40,
  value = 10,
  step = 1
)
binom.prob <- list(
  name = "prob",
  label_name = "Probability of successful trial",
  label_symbol = "p",
  min = 0,
  max = 1,
  value = 0.5,
  step = 0.01
)

## Instance ----
binom <- Distribution$new(
  dist = "binom",
  name = "Binomial distribution",
  wiki = "https://en.wikipedia.org/wiki/Binomial_distribution",
  c_or_d = "d",
  formula = binom.formula,
  func_p = binom.func_p,
  func_c = binom.func_c,
  x_filter = binom.x_filter,
  mean = binom.mean,
  mean_str = binom.mean_str,
  variance = binom.variance,
  variance_str = binom.variance_str,
  range = binom.range,
  binom.size,
  binom.prob
)
