# 超幾何分布
hyper.func <- function(m, n, k, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dhyper(x, m=m, n=n, k=k)
  } else {
    func <- function(x) phyper(x, m=m, n=n, k=k)
  }
  return(func)
}
