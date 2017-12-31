# ワイブル分布
weibull.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dweibull(x, shape=shape, scale=scale)
  } else {
    func <- function(x) pweibull(x, shape=shape, scale=scale)
  }
  return(func)
}
