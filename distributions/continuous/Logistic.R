# ロジスティック分布
logis.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlogis(x, location=location, scale=scale)
  } else {
    func <- function(x) plogis(x, location=location, scale=scale)
  }
  return(func)
}
