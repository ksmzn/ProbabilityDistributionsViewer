# F分布
f.func <- function(df1, df2, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) df(x, df1=df1, df2=df2, ncp=ncp)
  } else {
    func <- function(x) pf(x, df1=df1, df2=df2, ncp=ncp)
  }
  return(func)
}
