# 非心F分布
ncf.func <- function(df1, df2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) df(x, df1=df1, df2=df2, ncp=ncp)
  } else {
    func <- function(x) pf(x, df1=df1, df2=df2, ncp=ncp)
  }
  return(func)
}
