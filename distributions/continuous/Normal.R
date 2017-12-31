# 正規分布
norm.func <- function(mean, sd, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dnorm(x, mean=mean, sd=sd)
  } else {
    func <- function(x) pnorm(x, mean=mean, sd=sd)
  }
  return(func)
}
