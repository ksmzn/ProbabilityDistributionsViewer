# 一様分布
unif.func <- function(min, max, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dunif(x, min=min, max=max)
  } else {
    func <- function(x) punif(x, min=min, max=max)
  }
  return(func)
}
