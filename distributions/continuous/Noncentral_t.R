# 非心t分布
nct.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dt(x, df=df, ncp=ncp)
  } else {
    func <- function(x) pt(x, df=df, ncp=ncp)
  }
  return(func)
}
