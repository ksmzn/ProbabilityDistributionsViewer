# 非心ベータ分布
ncbeta.func <- function(shape1, shape2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dbeta(x, shape1=shape1, shape2=shape2, ncp=ncp)
  } else {
    func <- function(x) pbeta(x, shape1=shape1, shape2=shape2, ncp=ncp)
  }
  return(func)
}
