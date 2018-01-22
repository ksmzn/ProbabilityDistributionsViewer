# 超幾何分布 ----
## Functions ----
hyper.func_p <- function(m, n, k) function(x) dhyper(x, m=m, n=n, k=k)
hyper.func_c <- function(m, n, k) function(x) phyper(x, m=m, n=n, k=k)
hyper.formula <- "
\\operatorname{P}(X=x)
= \\frac{\\binom{m}{x}\\binom{n}{k-x}}{\\binom{m+n}{k}}
"

hyper.x_filter <- NULL

## Moments ----
hyper.mean <- function(m, n, k) k * m / (m + n)
hyper.mean_str <- "\\frac{km}{m+n}"
hyper.variance <- function(m, n, k) k * m * n * (m + n - k) / (((m + n) ** 2) * (m + n - 1))
hyper.variance_str <- "\\frac{kmn(m+n-k)}{(m+n)^2 (m+n-1)}"

## Parameters ----
hyper.range <- list(
  min = 0,
  max = 100,
  value = c(0, 50),
  step= 1
)
hyper.m <- list(
  name = "m",
  label_name = "Number of white balls in the urn",
  label_symbol = "m",
  min = 0,
  max = 100,
  value = 50,
  step = 1
)
hyper.n <- list(
  name = "n",
  label_name = "Number of black balls in the urn",
  label_symbol = "n",
  min = 0,
  max = 100,
  value = 50,
  step = 1
)
hyper.k <- list(
  name = "k",
  label_name = "Number of balls drawn from the urn",
  label_symbol = "k",
  min = 0,
  max = 100,
  value = 50,
  step = 1
)

## Instance ----
hyper <- Distribution$new(
  dist = "hyper",
  name = "Hypergeometric distribution",
  wiki = "https://en.wikipedia.org/wiki/Hypergeometric_distribution",
  c_or_d = "d",
  formula = hyper.formula,
  func_p = hyper.func_p,
  func_c = hyper.func_c,
  x_filter = hyper.x_filter,
  mean = hyper.mean,
  mean_str = hyper.mean_str,
  variance = hyper.variance,
  variance_str = hyper.variance_str,
  range = hyper.range,
  hyper.m,
  hyper.n,
  hyper.k
)
