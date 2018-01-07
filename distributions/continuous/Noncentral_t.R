# 非心t分布
## Functions ----
nct.func_p <- function(df, ncp) function(x) dt(x, df=df, ncp=ncp)
nct.func_c <- function(df, ncp) function(x) pt(x, df=df, ncp=ncp)
nct.formula <- "
f(x) =\\frac{\\nu^{\\frac{\\nu}{2}}
\\exp\\left (-\\frac{\\nu\\mu^2}{2(x^2+\\nu)} \\right )}
{\\sqrt{\\pi}\\Gamma(\\frac{\\nu}{2})2^{\\frac{\\nu-1}{2}}(x^2+\\nu)^{\\frac{\\nu+1}{2}}}
\\int_0^\\infty y^\\nu\\exp\\left (-\\frac{1}{2}\\left(y-\\frac{\\mu x}{\\sqrt{x^2+\\nu}}
\\right)^2\\right ) dy
"

nct.x_filter <- NULL

## Moments ----
nct.mean <- function(df, ncp){
  if(df <= 1){
    value <- NULL
  } else {
    value <- ncp * sqrt(df / 2) * gamma((df - 1) / 2) / gamma(df / 2)
  }
  return(value)
}
nct.mean_str <- "\\mu\\sqrt{\\frac{\\nu}{2}}\\frac{\\Gamma((\\nu-1)/2)}{\\Gamma(\\nu/2)}" 
nct.variance <- function(df, ncp) {
  if (df <= 2){
    value <- NULL
  } else {
    value <- (df * (1 + ncp ** 2)) / (df - 2) -
      ((ncp ** 2) * df / 2) * ((gamma((df - 1) / 2) / gamma(df / 2)) ** 2)
  }
  return(value)
}
nct.variance_str <- "\\frac{\\nu(1+\\mu^2)}{\\nu-2}-\\frac{\\mu^2\\nu}{2}\\left(\\frac{\\Gamma((\\nu-1)/2)}{\\Gamma(\\nu/2)}\\right)^2"

## Parameters ----
nct.range <- list(
  min = -100,
  max = 100,
  value = c(-10, 10),
  step= 0.5
)
nct.df <- list(
  name = "df",
  label = "自由度 \\(\\nu\\)",
  min = 1,
  max = 20,
  value = 1
)
nct.ncp <- list(
  name = "ncp",
  label = "非中心度 \\(\\mu\\)",
  min = 0,
  max = 20,
  value = 0,
  step = 0.1
)

## Instance ----
nct <- Distribution$new(
  dist = "nct",
  name = "Noncentral t-distribution",
  wiki = "https://en.wikipedia.org/wiki/Noncentral_t-distribution",
  c_or_d = "c",
  formula = nct.formula,
  func_p = nct.func_p,
  func_c = nct.func_c,
  x_filter = nct.x_filter,
  mean = nct.mean,
  mean_str = nct.mean_str,
  variance = nct.variance,
  variance_str = nct.variance_str,
  range = nct.range,
  nct.df,
  nct.ncp
)
