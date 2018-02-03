# F分布 ----
## Functions ----
f.func_p <- function(df1, df2) function(x) df(x, df1 = df1, df2 = df2, ncp = 0L)
f.func_c <- function(df1, df2) function(x) pf(x, df1 = df1, df2 = df2, ncp = 0L)
f.formula <- "
  f(x) = \\frac{1}{\\mathrm{B}(d_1/2, d_2/2)} \\; 
  \\left(\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_1/2} \\; 
  \\left(1-\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_2/2} \\; x^{-1}
"

f.x_filter <- NULL

## Moments ----
f.mean <- function(df1, df2) df2 / (df2 - 2)
f.mean_str <- "\\frac{d_2}{d_2-2}"
f.variance <- function(df1, df2) {
  (2 * df2 ** 2) * (df1 + df2 - 2) /
    (df1 * ((df2 - 2) ** 2) * (df2 - 4))
}
f.variance_str <- "\\frac{2\\,d_2^2\\,(d_1+d_2-2)}{d_1 (d_2-2)^2 (d_2-4)}"

## Parameters ----
f.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step = 0.5
)
f.df1 <- list(
  name = "df1",
  label_name = "Degrees of freedom",
  label_symbol = "d_1",
  min = 1,
  max = 20,
  value = 1
)
f.df2 <- list(
  name = "df2",
  label_name = "Degrees of freedom",
  label_symbol = "d_2",
  min = 1,
  max = 20,
  value = 1
)

## Instance ----
f <- Distribution$new(
  dist = "f",
  name = "F-distribution",
  wiki = "https://en.wikipedia.org/wiki/F-distribution",
  c_or_d = "c",
  formula = f.formula,
  func_p = f.func_p,
  func_c = f.func_c,
  x_filter = f.x_filter,
  mean = f.mean,
  mean_str = f.mean_str,
  variance = f.variance,
  variance_str = f.variance_str,
  range = f.range,
  f.df1,
  f.df2
)
