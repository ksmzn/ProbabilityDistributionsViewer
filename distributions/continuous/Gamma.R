# ガンマ分布
gamma.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dgamma(x, shape=shape, scale=scale)
  } else {
    func <- function(x) pgamma(x, shape=shape, scale=scale)
  }
  return(func)
}
