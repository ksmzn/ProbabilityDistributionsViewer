# 非心F分布
## Functions ----
ncf.func_p <- function(df1, df2, ncp) function(x) df(x, df1=df1, df2=df2, ncp=ncp)
ncf.func_c <- function(df1, df2, ncp) function(x) pf(x, df1=df1, df2=df2, ncp=ncp)
ncf.formula <- "
  f(x) =\\sum\\limits_{k=0}^\\infty
  \\frac{e^{-\\lambda/2}(\\lambda/2)^k}
  { B\\left(\\frac{\\nu_2}{2},\\frac{\\nu_1}{2}+k\\right) k!}
  \\left(\\frac{\\nu_1}{\\nu_2}\\right)^{\\frac{\\nu_1}{2}+k}
  \\left(\\frac{\\nu_2}{\\nu_2+\\nu_1x}\\right)
  ^{\\frac{\\nu_1+\\nu_2}{2}+k}x^{\\nu_1/2-1+k}
  \\ \\ \\ \\ \\mathrm{for\\ } x > 0
"

## Moments ----
ncf.mean <- function(df1, df2, ncp) {
  (df2 * (df1 + ncp)) / (df1 * (df2 - 2))
}
ncf.mean_str <- "\\frac{\\nu_2(\\nu_1 + \\lambda)}{\\nu_1(\\nu_2-2)}"

ncf.variance <- function(df1, df2, ncp) {
  (2 * df2 ** 2) * (df1 + df2 - 2) / (df1 * ((df2 - 2) ** 2) * (df2 - 4))
}
ncf.variance_str <- "2\\frac{(\\nu_1+\\lambda)^2+(\\nu_1+2\\lambda)(\\nu_2-2)}{(\\nu_2-2)^2(\\nu_2-4)} \\left(\\frac{\\nu_2}{\\nu_1}\\right)^2"

## Parameters ----
ncf.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 0.5
)
ncf.df1 <- list(
  name = "df1",
  label = "自由度 \\(\\nu_1\\)",
  min = 1,
  max = 20,
  value = 1
)
ncf.df2 <- list(
  name = "df2",
  label = "自由度 \\(\\nu_2\\)",
  min = 1,
  max = 20,
  value = 1
)
ncf.ncp <- list(
  name = "ncp",
  label = "非中心度 \\(\\lambda\\)",
  min = 0,
  max = 20,
  value = 0,
  step = 0.1
)

## Instance ----
ncf <- Distribution$new(
  dist = "ncf",
  name = "Noncentral F-distribution",
  wiki = "https://en.wikipedia.org/wiki/Noncentral_F-distribution",
  c_or_d = "c",
  func_p = ncf.func_p,
  func_c = ncf.func_c,
  formula = ncf.formula,
  mean = ncf.mean,
  mean_str = ncf.mean_str,
  variance = ncf.variance,
  variance_str = ncf.variance_str,
  range = ncf.range,
  ncf.df1,
  ncf.df2,
  ncf.ncp
)
