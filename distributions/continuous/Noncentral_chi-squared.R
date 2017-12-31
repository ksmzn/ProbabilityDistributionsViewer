# 非心カイ二乗分布
ncChisq.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df=df, ncp=ncp)
  } else {
    func <- function(x) pchisq(x, df=df, ncp=ncp)
  }
  return(func)
}
