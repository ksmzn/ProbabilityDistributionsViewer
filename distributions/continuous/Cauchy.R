# コーシー分布
cauchy.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dcauchy(x, location=location, scale=scale)
  } else {
    func <- function(x) pcauchy(x, location=location, scale=scale)
  }
  return(func)
}
