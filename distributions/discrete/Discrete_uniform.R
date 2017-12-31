# 離散一様分布
dunif.func <- function(min, max, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dunif(x, min, max)
  } else {
    func <- function(x) punif(x, min, max)
  }
  return(func)
}
