# 二項分布
binom.func <- function(size, prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dbinom(x, size=size, prob=prob)
  } else {
    func <- function(x) pbinom(x, size=size, prob=prob)
  }
  return(func)
}
