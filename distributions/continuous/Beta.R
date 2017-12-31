# ベータ分布
beta.func <- function(shape1, shape2, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dbeta(x, shape1=shape1, shape2=shape2, ncp=ncp)
  } else {
    func <- function(x) pbeta(x, shape1=shape1, shape2=shape2, ncp=ncp)
  }
  return(func)
}
