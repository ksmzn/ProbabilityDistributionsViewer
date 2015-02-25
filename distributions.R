library(shiny)

# To be called from server.R
####################################################
# 連続分布
####################################################
# F分布
f.func <- function(df1, df2, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) df(x, df1, df2, ncp)
  } else {
    func <- function(x) pf(x, df1, df2, ncp)
  }
  return(func)
}

# 非心F分布
ncf.func <- function(df1, df2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) df(x, df1, df2, ncp)
  } else {
    func <- function(x) pf(x, df1, df2, ncp)
  }
  return(func)
}

# カイ二乗分布
chisq.func <- function(df, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df, ncp)
  } else {
    func <- function(x) pchisq(x, df, ncp)
  }
  return(func)
}

# 非心カイ二乗分布
ncChisq.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df, ncp)
  } else {
    func <- function(x) pchisq(x, df, ncp)
  }
  return(func)
}

# ガンマ分布
gamma.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dgamma(x, shape, scale)
  } else {
    func <- function(x) pgamma(x, shape, scale)
  }
  return(func)
}

# コーシー分布
cauchy.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dcauchy(x, location, scale)
  } else {
    func <- function(x) pcauchy(x, location, scale)
  }
  return(func)
}

# 指数分布
exp.func <- function(rate, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dexp(x, rate)
  } else {
    func <- function(x) pexp(x, rate)
  }
  return(func)
}

# 正規分布
norm.func <- function(mean, sd, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dnorm(x, mean, sd)
  } else {
    func <- function(x) pnorm(x, mean, sd)
  }
  return(func)
}

# 対数正規分布
lnormal.func <- function(meanlog, sdlog, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlnorm(x, meanlog, sdlog)
  } else {
    func <- function(x) plnorm(x, meanlog, sdlog)
  }
  return(func)
}

# t分布
t.func <- function(df, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dt(x, df, ncp)
  } else {
    func <- function(x) pt(x, df, ncp)
  }
  return(func)
}

# 非心t分布
nct.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dt(x, df, ncp)
  } else {
    func <- function(x) pt(x, df, ncp)
  }
  return(func)
}

# ベータ分布
beta.func <- function(shape1, shape2, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dbeta(x, shape1, shape2, ncp)
  } else {
    func <- function(x) pbeta(x, shape1, shape2, ncp)
  }
  return(func)
}

# 非心ベータ分布
ncbeta.func <- function(shape1, shape2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dbeta(x, shape1, shape2, ncp)
  } else {
    func <- function(x) pbeta(x, shape1, shape2, ncp)
  }
  return(func)
}

# 一様分布
unif.func <- function(min, max, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dunif(x, min, max)
  } else {
    func <- function(x) punif(x, min, max)
  }
  return(func)
}

# ロジスティック分布
logis.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlogis(x, location, scale)
  } else {
    func <- function(x) plogis(x, location, scale)
  }
  return(func)
}

# ワイブル分布
weibull.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dweibull(x, shape, scale)
  } else {
    func <- function(x) pweibull(x, shape, scale)
  }
  return(func)
}

####################################################
# 離散分布
####################################################
# 幾何分布
geom.func <- function(prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dgeom(x, prob)
  } else {
    func <- function(x) pgeom(x, prob)
  }
  return(func)
}

# 超幾何分布
hyper.func <- function(m, n, k, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dhyper(x, m, n, k)
  } else {
    func <- function(x) phyper(x, m, n, k)
  }
  return(func)
}

# 二項分布
binom.func <- function(size, prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dbinom(x, size, prob)
  } else {
    func <- function(x) pbinom(x, size, prob)
  }
  return(func)
}

# 負の二項分布
nbinom.func <- function(size, prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dnbinom(x, size, prob)
  } else {
    func <- function(x) pnbinom(x, size, prob)
  }
  return(func)
}

# ポアソン分布
pois.func <- function(lambda, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dpois(x, lambda)
  } else {
    func <- function(x) ppois(x, lambda)
  }
  return(func)
}

# # 離散一様分布
# dunif.func <- function(min, max, p_or_c){
#   if(p_or_c == "p"){
#     func <- function(x) dunif(x, min, max)
#   } else {
#     func <- function(x) punif(x, min, max)
#   }
#   return(func)
# }

