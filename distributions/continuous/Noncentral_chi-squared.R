# 非心カイ二乗分布
ncChisq.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df=df, ncp=ncp)
  } else {
    func <- function(x) pchisq(x, df=df, ncp=ncp)
  }
  return(func)
}
## Functions ----
ncChisq.func_p <- function(df, ncp) function(x) dchisq(x, df=df, ncp=ncp)
ncChisq.func_c <- function(df, ncp) function(x) pchisq(x, df=df, ncp=ncp)
ncChisq.formula <- "f_X(x; k,\\lambda) =
        \\sum_{i=0}^\\infty \\frac{e^{-\\lambda/2} (\\lambda/2)^i}{i!} f_{Y_{k+2i}}(x),
        \\ \\ \\ \\ \\mathrm{for\\ } x > 0,\\ \\ Y_q \\sim\\chi^2_q \\
"

ncChisq.x_filter <- function(x, df, ncp) {
  if (df == 1) {
    x <- x[x!=0]
  }
  return(x)
}

## Moments ----
ncChisq.mean <- function(df, ncp) df + ncp
ncChisq.mean_str <- "k+\\lambda"
ncChisq.variance <- function(df, ncp) 2 * (df + 2 * ncp)
ncChisq.variance_str <- "2(k+2\\lambda)"

## Parameters ----
ncChisq.range <- list(
  min = 0,
  max = 100,
  value = c(0, 20),
  step= 0.5
)
ncChisq.df <- list(
  name = "df",
  label = "自由度 \\(k\\)",
  min = 1,
  max = 20,
  value = 1
)
ncChisq.ncp <- list(
  name = "ncp",
  label = "非中心度 \\(\\lambda\\)",
  min = 0,
  max = 20,
  value = 0,
  step = 0.1
)

## Instance ----
ncChisq <- Distribution$new(
  dist = "ncChisq",
  name = "Noncentral chi-squared distribution",
  wiki = "https://en.wikipedia.org/wiki/Noncentral_chi-squared_distribution",
  c_or_d = "c",
  func_p = ncChisq.func_p,
  func_c = ncChisq.func_c,
  formula = ncChisq.formula,
  x_filter = ncChisq.x_filter,
  mean = ncChisq.mean,
  mean_str = ncChisq.mean_str,
  variance = ncChisq.variance,
  variance_str = ncChisq.variance_str,
  range = ncChisq.range,
  ncChisq.df,
  ncChisq.ncp
)
