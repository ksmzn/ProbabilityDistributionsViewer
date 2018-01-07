# 幾何分布 ----
## Functions ----
geom.func_p <- function(prob) function(x) dgeom(x, prob=prob)
geom.func_c <- function(prob) function(x) pgeom(x, prob=prob)
geom.formula <- "
Pr(X = k) = p(1-p)^{k}
"

geom.x_filter <- NULL

## Moments ----
geom.mean <- function(prob) (1 - prob) / prob
geom.mean_str <- "\\frac{1-p}{p}"
geom.variance <- function(prob) (1 - prob) / prob ** 2
geom.variance_str <- "\\frac{1-p}{p^2}"

## Parameters ----
geom.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 1
)
geom.prob <- list(
  name = "prob",
  label = "成功確率 \\(p\\)",
  min = 0,
  max = 1,
  value = 0.5,
  step = 0.01
)

## Instance ----
geom <- Distribution$new(
  dist = "geom",
  name = "Geometric distribution",
  wiki = "https://en.wikipedia.org/wiki/Geometric_distribution",
  c_or_d = "d",
  formula = geom.formula,
  func_p = geom.func_p,
  func_c = geom.func_c,
  x_filter = geom.x_filter,
  mean = geom.mean,
  mean_str = geom.mean_str,
  variance = geom.variance,
  variance_str = geom.variance_str,
  range = geom.range,
  geom.prob
)
