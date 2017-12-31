# 幾何分布
geom.func <- function(prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dgeom(x, prob=prob)
  } else {
    func <- function(x) pgeom(x, prob=prob)
  }
  return(func)
}
