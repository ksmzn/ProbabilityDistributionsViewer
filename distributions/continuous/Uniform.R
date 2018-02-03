# 一様分布 ----
## functions ----
unif.func_p <- function(min, max) function(x) dunif(x, min = min, max = max)
unif.func_c <- function(min, max) function(x) punif(x, min = min, max = max)
unif.formula <- "
f(x)=\\begin{cases}
\\frac{1}{b - a} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
\\end{cases} 
"

unif.x_filter <- NULL

## Moments ----
unif.mean <- function(min, max) (min + max) / 2
unif.mean_str <- "\\frac{1}{2}(a+b)"
unif.variance <- function(min, max) ((max - min) ** 2) / 12
unif.variance_str <- "\\frac{1}{12}(b-a)^2"

## Parameters ----
unif.range <- list(
  min = -50,
  max = 50,
  value = c(0, 1),
  step = 0.5
)
unif.min <- list(
  name = "min",
  label_name = "Min",
  label_symbol = "a",
  min = -50,
  max = 50,
  value = 0,
  step = 0.5
)
unif.max <- list(
  name = "max",
  label_name = "Max",
  label_symbol = "b",
  min = -50,
  max = 50,
  value = 1,
  step = 0.5
)

## Instance ----
unif <- Distribution$new(
  dist = "unif",
  name = "Uniform distribution (continuous)",
  wiki = "https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)",
  c_or_d = "c",
  formula = unif.formula,
  func_p = unif.func_p,
  func_c = unif.func_c,
  x_filter = unif.x_filter,
  mean = unif.mean,
  mean_str = unif.mean_str,
  variance = unif.variance,
  variance_str = unif.variance_str,
  range = unif.range,
  unif.min,
  unif.max
)
