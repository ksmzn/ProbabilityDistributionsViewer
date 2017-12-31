# 負の二項分布
nbinom.func <- function(size, prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dnbinom(x, size=size, prob=prob)
  } else {
    func <- function(x) pnbinom(x, size=size, prob=prob)
  }
  return(func)
}
