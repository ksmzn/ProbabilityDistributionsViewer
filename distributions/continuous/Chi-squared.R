# カイ二乗分布
## Functions ----
chisq.func_p <- function(df) function(x) dchisq(x, df=df, ncp=0L)
chisq.func_c <- function(df) function(x) pchisq(x, df=df, ncp=0L)
chisq.formula <- "
  f(x;k)=\\frac{(1/2)^{k/2}}{\\Gamma(k/2)} x^{k/2 - 1} e^{-x/2}
  \\ \\ \\ \\ \\mathrm{for\\ } x > 0
"

## Moments ----
chisq.mean <- function(df) df
chisq.mean_str <- "k"
chisq.variance <- function(df) 2 * df
chisq.variance_str <- "2k"

## Parameters ----
chisq.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 0.5
)
chisq.df <- list(
  name = "df",
  label = "自由度 \\(k\\)",
  min = 1,
  max = 20,
  value = 1
)

## Instance ----
chisq <- Distribution$new(
  dist = "chisq",
  name = "Chi-squared distribution",
  wiki = "https://en.wikipedia.org/wiki/Noncentral_chi-squared_distribution",
  c_or_d = "c",
  formula = chisq.formula,
  func_p = chisq.func_p,
  func_c = chisq.func_c,
  mean = chisq.mean,
  mean_str = chisq.mean_str,
  variance = chisq.variance,
  variance_str = chisq.variance_str,
  range = chisq.range,
  chisq.df
)
