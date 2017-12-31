# 指数分布
exp.func <- function(rate, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dexp(x, rate=rate)
  } else {
    func <- function(x) pexp(x, rate=rate)
  }
  return(func)
}
