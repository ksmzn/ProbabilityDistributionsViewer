library(shiny)
library(ggplot2)

# F分布
f.func <- function(df1, df2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) df(x, df1, df2, ncp)
  } else {
    func <- function(x) pf(x, df1, df2, ncp)
  }
  return(func)
}

# カイ二乗分布
chisq.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df, ncp)
  } else {
    func <- function(x) pchisq(x, df, ncp)
  }
  return(func)
}

# ガンマ分布
gamma.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dgamma(x, shape, scale)
  } else {
    func <- function(x) pgamma(x, shape, scale)
  }
  return(func)
}

# コーシー分布
cauchy.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dcauchy(x, location, scale)
  } else {
    func <- function(x) pcauchy(x, location, scale)
  }
  return(func)
}

# 指数分布
exp.func <- function(rate, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dexp(x, rate)
  } else {
    func <- function(x) pexp(x, rate)
  }
  return(func)
}

# 正規分布
norm.func <- function(mean, sd, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dnorm(x, mean, sd)
  } else {
    func <- function(x) pnorm(x, mean, sd)
  }
  return(func)
}

# 対数正規分布
lnorm.func <- function(meanlog, sdlog, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlnorm(x, meanlog, sdlog)
  } else {
    func <- function(x) plnorm(x, meanlog, sdlog)
  }
  return(func)
}

# t分布
t.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dt(x, df, ncp)
  } else {
    func <- function(x) pt(x, df, ncp)
  }
  return(func)
}

# ベータ分布
beta.func <- function(shape1, shape2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dbeta(x, shape1, shape2, ncp)
  } else {
    func <- function(x) pbeta(x, shape1, shape2, ncp)
  }
  return(func)
}

# 一様分布
unif.func <- function(min, max, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dunif(x, min, max)
  } else {
    func <- function(x) punif(x, min, max)
  }
  return(func)
}

# ロジスティック分布
logis.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlogis(x, location, scale)
  } else {
    func <- function(x) plogis(x, location, scale)
  }
  return(func)
}

# ワイブル分布
weibull.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dweibull(x, shape, scale)
  } else {
    func <- function(x) pweibull(x, shape, scale)
  }
  return(func)
}

# サーバロジックの定義。ヒストグラムを描く
shinyServer(function(input, output) {
  # ヒストグラムを描くための式。
  # この式は renderPlot にラップされている。つまり、
  #
  #  1) これは "reactive" であり、入力が変更されると
  #     自動的に再実行される
  #  2) この出力タイプは plot である
  output$homePlot <- renderPlot({
    func <- norm.func(input$home.mean, input$home.sd, input$home.p_or_c)
    p <- ggplot(data.frame(x=input$home.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$fPlot <- renderPlot({
    func <- f.func(input$f.df1, input$f.df2, input$f.ncp, input$f.p_or_c)
    p <- ggplot(data.frame(x=input$f.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$chisqPlot <- renderPlot({
    func <- chisq.func(input$chisq.df, input$chisq.ncp, input$chisq.p_or_c)
    p <- ggplot(data.frame(x=input$chisq.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$gammaPlot <- renderPlot({
    func <- gamma.func(input$gamma.shape, input$gamma.scale, input$gamma.p_or_c)
    p <- ggplot(data.frame(x=input$gamma.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$cauchyPlot <- renderPlot({
    func <- cauchy.func(input$cauchy.location, input$cauchy.scale, input$cauchy.p_or_c)
    p <- ggplot(data.frame(x=input$cauchy.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$expPlot <- renderPlot({
    func <- exp.func(input$exp.rate, input$exp.p_or_c)
    p <- ggplot(data.frame(x=input$exp.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$normalPlot <- renderPlot({
    func <- norm.func(input$norm.mean, input$norm.sd, input$norm.p_or_c)
    p <- ggplot(data.frame(x=input$norm.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$lnormalPlot <- renderPlot({
    func <- lnorm.func(input$lnorm.meanlog, input$lnorm.sdlog, input$lnorm.p_or_c)
    p <- ggplot(data.frame(x=input$lnorm.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$tPlot <- renderPlot({
    func <- t.func(input$t.df, input$t.ncp, input$t.p_or_c)
    p <- ggplot(data.frame(x=input$t.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$betaPlot <- renderPlot({
    func <- beta.func(input$beta.shape1, input$beta.shape2, input$beta.ncp, input$beta.p_or_c)
    p <- ggplot(data.frame(x=input$beta.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$unifPlot <- renderPlot({
    func <- unif.func(input$unif.range[1], input$unif.range[2], input$unif.p_or_c)
    p <- ggplot(data.frame(x=input$unif.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$logisPlot <- renderPlot({
    func <- logis.func(input$logis.location, input$logis.scale, input$logis.p_or_c)
    p <- ggplot(data.frame(x=input$logis.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$weibullPlot <- renderPlot({
    func <- weibull.func(input$weibull.shape, input$weibull.scale, input$weibull.p_or_c)
    p <- ggplot(data.frame(x=input$weibull.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })
})
