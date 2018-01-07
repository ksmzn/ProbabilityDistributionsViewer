# t分布 ----
## Functions ----
t_dist.func_p <- function(df) function(x) dt(x, df=df, ncp=0L)
t_dist.func_c <- function(df) function(x) pt(x, df=df, ncp=0L)
t_dist.formula <- "
f(x) = \\frac{\\Gamma((\\nu+1)/2)}{\\sqrt{\\nu\\pi\\,}\\,
  \\Gamma(\\nu/2)} (1+x^2/\\nu)^{-(\\nu+1)/2}
"

## Moments ----
t_dist.mean <- function(df) 0L
t_dist.mean_str <- "e^{\\mu+\\sigma^2/2}"
t_dist.variance <- function(df) {
  if (df <= 2){
    return(Inf)
  } else {
    return(df / (df - 2))
  }
}
t_dist.variance_str <- "\\frac{\\nu}{\\nu-2}"

## Parameters ----
t_dist.range <- list(
  min = -100,
  max = 100,
  value = c(-10, 10),
  step= 0.5
)
t_dist.df <- list(
  name = "df",
  label = "自由度 \\(\\nu\\)",
  min = 1,
  max = 20,
  value = 1
)

## Instance ----
t_dist <- Distribution$new(
  dist = "t_dist",
  name = "Student's t-distribution",
  wiki = "https://en.wikipedia.org/wiki/Student%27s_t-distribution",
  c_or_d = "c",
  formula = t_dist.formula,
  func_p = t_dist.func_p,
  func_c = t_dist.func_c,
  mean = t_dist.mean,
  mean_str = t_dist.mean_str,
  variance = t_dist.variance,
  variance_str = t_dist.variance_str,
  range = t_dist.range,
  t_dist.df
)
