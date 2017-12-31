# 対数正規分布
lnormal.func <- function(meanlog, sdlog, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlnorm(x, meanlog, sdlog)
  } else {
    func <- function(x) plnorm(x, meanlog, sdlog)
  }
  return(func)
}

