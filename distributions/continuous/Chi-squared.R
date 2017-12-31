# カイ二乗分布
chisq.func <- function(df, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df=df, ncp=ncp)
  } else {
    func <- function(x) pchisq(x, df=df, ncp=ncp)
  }
  return(func)
}
