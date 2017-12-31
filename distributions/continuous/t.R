# t分布
t.func <- function(df, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dt(x, df=df, ncp=ncp)
  } else {
    func <- function(x) pt(x, df=df, ncp=ncp)
  }
  return(func)
}
