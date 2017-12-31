# ポアソン分布
pois.func <- function(lambda, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dpois(x, lambda=lambda)
  } else {
    func <- function(x) ppois(x, lambda=lambda)
  }
  return(func)
}
