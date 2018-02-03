# ベータ分布 ----
## functions ----
beta.func_p <- function(shape1, shape2) function(x) dbeta(x, shape1 = shape1, shape2 = shape2, ncp = 0L)
beta.func_c <- function(shape1, shape2) function(x) pbeta(x, shape1 = shape1, shape2 = shape2, ncp = 0L)
beta.formula <- "
f(x)=\\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha,\\beta)}
"

beta.x_filter <- function(x, shape1, shape2) {
  if (shape1 < 1) {
    x <- x[x != 0]
  }
  if (shape2 < 1) {
    x <- x[x != 1]
  }
  return(x)
}

## Moments ----
beta.mean <- function(shape1, shape2) shape1 / (shape1 + shape2)
beta.mean_str <- "\\frac{\\alpha}{\\alpha+\\beta}"
beta.variance <- function(shape1, shape2) {
  (shape1 * shape2) /
    (((shape1 + shape2) ** 2) *
      (shape1 + shape1 + 1))
}
beta.variance_str <- "\\frac{\\alpha\\beta}{(\\alpha+\\beta)^2 (\\alpha+\\beta+1)}"

## Parameters ----
beta.range <- list(
  min = 0,
  max = 1,
  value = c(0, 1),
  step = 0.01
)
beta.shape1 <- list(
  name = "shape1",
  label_name = "Shape",
  label_symbol = "\\alpha",
  min = 0,
  max = 20,
  value = 2,
  step = 0.1
)
beta.shape2 <- list(
  name = "shape2",
  label_name = "Shape",
  label_symbol = "\\beta",
  min = 0,
  max = 20,
  value = 2,
  step = 0.1
)

## Instance ----
beta <- Distribution$new(
  dist = "beta",
  name = "Beta distribution",
  wiki = "https://en.wikipedia.org/wiki/Beta_distribution",
  c_or_d = "c",
  func_p = beta.func_p,
  func_c = beta.func_c,
  formula = beta.formula,
  x_filter = beta.x_filter,
  mean = beta.mean,
  mean_str = beta.mean_str,
  variance = beta.variance,
  variance_str = beta.variance_str,
  range = beta.range,
  beta.shape1,
  beta.shape2
)
