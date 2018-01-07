library(shiny)
library(hypergeo)

boxcolor <- "blue"
mean.icon <- icon("star", lib = "glyphicon")
variance.icon <- icon("resize-horizontal", lib = "glyphicon")

# Functions ----
createFormula <- function(f_str, value){
  paste0("\\(", f_str, "\\!=\\!", value, "\\)") 
}

createBox <- function(f_str, value, param_str, param = "mean"){
  if(param == "variance"){
    icon <- variance.icon
  } else {
    icon <- mean.icon
  }
  if(is.null(f_str) | is.null(value)){
    formula <- "定義されない"
  } else {
    value <- round(value, digits = 3)
    formula <- createFormula(f_str, value)
    formula <- withMathJax(formula)
  }
  box <-
    valueBox(
      formula,
      param_str,
      icon = icon,
      color = boxcolor
    )
  return(box)
}

meanBox <- function(f_str, value, param_str = "期待値"){
  f <- createBox(f_str, value, param_str, param = "mean")
  return(f)
}

varianceBox <- function(f_str, value, param_str = "分散"){
  f <- createBox(f_str, value, param_str, param = "variance")
  return(f)
}

server <- function(input, output) {
  ###########################################################################
  # 正規分布
  ###########################################################################
  output$norm.meanBox <- renderValueBox({
    value <- norm$mean(input$norm.mean, input$norm.sd)
    f_str <- norm$mean_str
    meanBox(f_str, value)
  })

  output$norm.varianceBox <- renderValueBox({
    value <- norm$variance(input$norm.mean, input$norm.sd)
    f_str <- norm$variance_str
    varianceBox(f_str, value)
  })
  # outputs for plot
  output$normPlot <- renderNvd3Chart({
    func <- norm$func(input$norm.p_or_c)(input$norm.mean, input$norm.sd)
    x<-seq(input$norm.range[1], input$norm.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })
  ###########################################################################
  # アーラン分布
  ###########################################################################
  output$erlang.meanBox <- renderValueBox({
    value <- erlang$mean(input$erlang.shape, input$erlang.scale)
    f_str <- erlang$mean_str
    meanBox(f_str, value)
  })

  output$erlang.varianceBox <- renderValueBox({
    value <- erlang$variance(input$erlang.shape, input$erlang.scale)
    f_str <- erlang$variance_str
    varianceBox(f_str, value)
  })

  # outputs for plot
  output$erlangPlot <- renderNvd3Chart({
    func <- erlang$func(input$erlang.p_or_c)(input$erlang.shape, input$erlang.scale)
    x<-seq(input$erlang.range[1], input$erlang.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # F分布
  ###########################################################################
  output$f.meanBox <- renderValueBox({
    value <- f$mean(input$f.df2, input$f.df2)
    f_str <- f$mean_str
    meanBox(f_str, value)
  })

  output$f.varianceBox <- renderValueBox({
    value <- f$variance(input$f.df1, input$f.df2)
    f_str <- f$variance_str
    varianceBox(f_str, value)
  })

  # outputs for plot
  output$fPlot <- renderNvd3Chart({
    func <- f$func(input$f.p_or_c)(input$f.df1, input$f.df2)
    x<-seq(input$f.range[1], input$f.range[2] ,length=1000)
    if (input$f.df1 == 1) {
      x <- x[x!=0]
    }
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 非心F分布
  ###########################################################################
  output$ncf.meanBox <- renderValueBox({
    value <- ncf$mean(input$ncf.df1, input$ncf.df2, input$ncf.ncp)
    f_str <- ncf$mean_str
    meanBox(f_str, value)
  })

  output$ncf.varianceBox <- renderValueBox({
    value <- ncf$variance(input$ncf.df1, input$ncf.df2, input$ncf.ncp)
    f_str <- ncf$variance_str
    varianceBox(f_str, value)
  })

  # outputs for plot
  output$ncfPlot <- renderNvd3Chart({
    func <- ncf$func(input$ncf.p_or_c)(input$ncf.df1, input$ncf.df2, input$ncf.ncp)
    x<-seq(input$ncf.range[1], input$ncf.range[2] ,length=1000)
    if (input$ncf.df1 == 1) {
      x <- x[x!=0]
    }
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })
  ###########################################################################
  # カイ二乗分布
  ###########################################################################
  output$chisq.meanBox <- renderValueBox({
    value <- chisq$mean(input$chisq.df)
    f_str <- chisq$mean_str
    meanBox(f_str, value)
  })

  output$chisq.varianceBox <- renderValueBox({
    value <- chisq$variance(input$chisq.df)
    f_str <- chisq$variance_str
    varianceBox(f_str, value)
  })

  output$chisqPlot <- renderNvd3Chart({
    func <- chisq$func(input$chisq.p_or_c)(input$chisq.df)
    x<-seq(input$chisq.range[1], input$chisq.range[2] ,length=1000)
    if (input$chisq.df == 1) {
      x <- x[x!=0]
    }
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 非心カイ二乗分布
  ###########################################################################
  output$ncChisq.meanBox <- renderValueBox({
    value <- ncChisq$mean(input$ncChisq.df, input$ncChisq.ncp)
    f_str <- ncChisq$mean_str
    meanBox(f_str, value)
  })

  output$ncChisq.varianceBox <- renderValueBox({
    value <- ncChisq$variance(input$ncChisq.df, input$ncChisq.ncp)
    f_str <- ncChisq$variance_str
    varianceBox(f_str, value)
  })

  output$ncChisqPlot <- renderNvd3Chart({
    func <- ncChisq$func(input$ncChisq.p_or_c)(input$ncChisq.df, input$ncChisq.ncp)
    x<-seq(input$ncChisq.range[1], input$ncChisq.range[2] ,length=1000)
    if (input$ncChisq.df == 1) {
      x <- x[x!=0]
    }
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # ガンマ分布
  ###########################################################################
  output$gamma.meanBox <- renderValueBox({
    value <- gamma$mean(input$gamma.shape, input$gamma.scale)
    f_str <- gamma$mean_str
    meanBox(f_str, value)
  })

  output$gamma.varianceBox <- renderValueBox({
    value <- gamma$variance(input$gamma.shape, input$gamma.scale)
    f_str <- gamma$variance_str
    varianceBox(f_str, value)
  })

  output$gammaPlot <- renderNvd3Chart({
    func <- gamma$func(input$gamma.p_or_c)(input$gamma.shape, input$gamma.scale)
    x<-seq(input$gamma.range[1], input$gamma.range[2] ,length=1000)
    if (input$gamma.shape < 1) {
      x <- x[x!=0]
    }
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # コーシー分布
  ###########################################################################
  output$cauchy.meanBox <- renderValueBox({
    value <- cauchy$mean()
    f_str <- cauchy$mean_str
    meanBox(f_str, value)
  })

  output$cauchy.varianceBox <- renderValueBox({
    value <- cauchy$variance()
    f_str <- cauchy$variance_str
    varianceBox(f_str, value)
  })

  output$cauchyPlot <- renderNvd3Chart({
    func <- cauchy$func(input$cauchy.p_or_c)(input$cauchy.location, input$cauchy.scale)
    x<-seq(input$cauchy.range[1], input$cauchy.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 指数分布
  ###########################################################################
  output$exp_dist.meanBox <- renderValueBox({
    value <- exp_dist$mean(input$exp_dist.rate)
    f_str <- exp_dist$mean_str
    meanBox(f_str, value)
  })

  output$exp_dist.varianceBox <- renderValueBox({
    value <- exp_dist$variance(input$exp_dist.rate)
    f_str <- exp_dist$variance_str
    varianceBox(f_str, value)
  })

  output$exp_distPlot <- renderNvd3Chart({
    func <- exp_dist$func(input$exp_dist.p_or_c)(input$exp_dist.rate)
    x<-seq(input$exp_dist.range[1], input$exp_dist.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 対数正規分布
  ###########################################################################
  output$lnormal.meanBox <- renderValueBox({
    value <- lnormal$mean(input$lnormal.meanlog, input$lnormal.sdlog)
    f_str <- lnormal$mean_str
    meanBox(f_str, value)
  })

  output$lnormal.varianceBox <- renderValueBox({
    value <- lnormal$variance(input$lnormal.meanlog, input$lnormal.sdlog)
    f_str <- lnormal$variance_str
    varianceBox(f_str, value)
  })

  output$lnormalPlot <- renderNvd3Chart({
    func <- lnormal$func(input$lnormal.p_or_c)(input$lnormal.meanlog, input$lnormal.sdlog)
    x<-seq(input$lnormal.range[1], input$lnormal.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # t分布
  ###########################################################################
  output$t_dist.meanBox <- renderValueBox({
    value <- t_dist$mean(input$t_dist.df)
    f_str <- t_dist$mean_str
    meanBox(f_str, value)
  })

  output$t_dist.varianceBox <- renderValueBox({
    value <- t_dist$variance(input$t_dist.df)
    f_str <- t_dist$variance_str
    varianceBox(f_str, value)
  })

  output$t_distPlot <- renderNvd3Chart({
    func <- t_dist$func(input$t_dist.p_or_c)(input$t_dist.df)
    x<-seq(input$t_dist.range[1], input$t_dist.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 非心t分布
  ###########################################################################
  output$nct.meanBox <- renderValueBox({
    value <- nct$mean(input$nct.df, input$nct.ncp)
    f_str <- nct$mean_str
    meanBox(f_str, value)
  })

  output$nct.varianceBox <- renderValueBox({
    value <- nct$variance(input$nct.df, input$nct.ncp)
    f_str <- nct$variance_str
    varianceBox(f_str, value)
  })

  output$nctPlot <- renderNvd3Chart({
    func <- nct$func(input$nct.p_or_c)(input$nct.df, input$nct.ncp)
    x<-seq(input$nct.range[1], input$nct.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # ベータ分布
  ###########################################################################
  output$beta.meanBox <- renderValueBox({
    value <- beta$mean(input$beta.shape1, input$beta.shape2)
    f_str <- beta$mean_str
    meanBox(f_str, value)
  })

  output$beta.varianceBox <- renderValueBox({
    value <- beta$variance(input$beta.shape1, input$beta.shape2)
    f_str <- beta$variance_str
    varianceBox(f_str, value)
  })

  output$betaPlot <- renderNvd3Chart({
    func <- beta$func(input$beta.p_or_c)(input$beta.shape1, input$beta.shape2)
    x<-seq(input$beta.range[1], input$beta.range[2] ,length=1000)
    if (input$beta.shape1 < 1) {
      x <- x[x!=0]
    }
    if (input$beta.shape2 < 1) {
      x <- x[x!=1]
    }
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })
  ###########################################################################
  # 非心ベータ分布
  ###########################################################################
  output$ncbeta.meanBox <- renderValueBox({
    value <- ncbeta$mean(input$ncbeta.shape1, input$ncbeta.shape2, input$ncbeta.ncp)
    f_str <- ncbeta$mean_str
    meanBox(f_str, value)
  })

  output$ncbeta.varianceBox <- renderValueBox({
    value <- ncbeta$variance(input$ncbeta.shape1, input$ncbeta.shape2, input$ncbeta.ncp)
    f_str <- ncbeta$variance_str
    varianceBox(f_str, value)
  })

  output$ncbetaPlot <- renderNvd3Chart({
    func <- ncbeta$func(input$ncbeta.p_or_c)(
      input$ncbeta.shape1,
      input$ncbeta.shape2,
      input$ncbeta.ncp)
    x <- seq(input$ncbeta.range[1], input$ncbeta.range[2] ,length=1000)
    if (input$ncbeta.shape1 < 1) {
      x <- x[x!=0]
    }
    if (input$ncbeta.shape2 < 1) {
      x <- x[x!=1]
    }
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 連続一様分布
  ###########################################################################
  output$unif.meanBox <- renderValueBox({
    value <- unif$mean(input$unif.range[1], input$unif.range[2])
    f_str <- unif$mean_str
    meanBox(f_str, value)
  })

  output$unif.varianceBox <- renderValueBox({
    value <- unif$variance(input$unif.range[1], input$unif.range[2])
    f_str <- unif$variance_str
    varianceBox(f_str, value)
  })

  output$unifPlot <- renderNvd3Chart({
    func <- unif$func(input$unif.p_or_c)(input$unif.range[1], input$unif.range[2])
    x<-seq(input$unif.range[1], input$unif.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # ロジスティック分布
  ###########################################################################
  output$logis.meanBox <- renderValueBox({
    value <- logis$mean(input$logis.location, input$logis.scale)
    f_str <- logis$mean_str
    meanBox(f_str, value)
  })

  output$logis.varianceBox <- renderValueBox({
    value <- logis$variance(input$logis.location, input$logis.scale)
    f_str <- logis$variance_str
    varianceBox(f_str, value)
  })

  output$logisPlot <- renderNvd3Chart({
    func <- logis.func(input$logis.location, input$logis.scale, input$logis.p_or_c)
    x<-seq(input$logis.range[1], input$logis.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })
  ###########################################################################
  # ワイブル分布
  ###########################################################################
  output$weibull.meanBox <- renderValueBox({
    value <- weibull$mean(input$weibull.shape, input$weibull.scale)
    f_str <- weibull$mean_str
    meanBox(f_str, value)
  })

  output$weibull.varianceBox <- renderValueBox({
    value <- weibull$variance(input$weibull.shape, input$weibull.scale)
    f_str <- weibull$variance_str
    varianceBox(f_str, value)
  })

  output$weibullPlot <- renderNvd3Chart({
    func <- weibull$func(input$weibull.p_or_c)(input$weibull.shape, input$weibull.scale)
    x<-seq(input$weibull.range[1], input$weibull.range[2] ,length=1000)
    if (input$weibull.shape < 1) {
      x <- x[x!=0]
    }
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 幾何分布
  ###########################################################################
  output$geom.meanBox <- renderValueBox({
    value <- geom$mean(input$geom.prob)
    f_str <- geom$mean_str
    meanBox(f_str, value)
  })

  output$geom.varianceBox <- renderValueBox({
    value <- geom$variance(input$geom.prob)
    f_str <- geom$variance_str
    varianceBox(f_str, value)
  })

  output$geomPlot <- renderNvd3Chart({
    func <- geom$func(input$geom.p_or_c)(input$geom.prob)
    x<-seq(input$geom.range[1], input$geom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 超幾何分布
  ###########################################################################
  output$hyper.meanBox <- renderValueBox({
    value <- hyper$mean(input$hyper.m, input$hyper.n, input$hyper.k)
    f_str <- hyper$mean_str
    meanBox(f_str, value)
  })

  output$hyper.varianceBox <- renderValueBox({
    value <- hyper$variance(input$hyper.m, input$hyper.n, input$hyper.k)
    f_str <- hyper$variance_str
    varianceBox(f_str, value)
  })

  output$hyperPlot <- renderNvd3Chart({
    func <- hyper$func(input$hyper.p_or_c)(input$hyper.m, input$hyper.n, input$hyper.k)
    x<-seq(input$hyper.range[1], input$hyper.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 二項分布
  ###########################################################################
  output$binom.meanBox <- renderValueBox({
    value <- binom$mean(input$binom.size, input$binom.prob)
    f_str <- binom$mean_str
    meanBox(f_str, value)
  })

  output$binom.varianceBox <- renderValueBox({
    value <- binom$variance(input$binom.size, input$binom.prob)
    f_str <- binom$variance_str
    varianceBox(f_str, value)
  })

  output$binomPlot <- renderNvd3Chart({
    func <- binom$func(input$binom.p_or_c)(input$binom.size, input$binom.prob)
    x<-seq(input$binom.range[1], input$binom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 負の二項分布
  ###########################################################################
  output$nbinom.meanBox <- renderValueBox({
    value <- nbinom$mean(input$nbinom.size, input$nbinom.prob)
    f_str <- nbinom$mean_str
    meanBox(f_str, value)
  })

  output$nbinom.varianceBox <- renderValueBox({
    value <- nbinom$variance(input$nbinom.size, input$nbinom.prob)
    f_str <- nbinom$variance_str
    varianceBox(f_str, value)
  })

  output$nbinomPlot <- renderNvd3Chart({
    func <- nbinom$func(input$nbinom.p_or_c)(input$nbinom.size, input$nbinom.prob)
    x<-seq(input$nbinom.range[1], input$nbinom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # ポアソン分布
  ###########################################################################
  output$pois.meanBox <- renderValueBox({
    value <- pois$mean(input$pois.lambda)
    f_str <- pois$mean_str
    meanBox(f_str, value)
  })

  output$pois.varianceBox <- renderValueBox({
    value <- pois$variance(input$pois.lambda)
    f_str <- pois$variance_str
    varianceBox(f_str, value)
  })

  output$poisPlot <- renderNvd3Chart({
    func <- pois$func(input$pois.p_or_c)(input$pois.lambda)
    x<-seq(input$pois.range[1], input$pois.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 連続一様分布
  ###########################################################################
  output$dunif.meanBox <- renderValueBox({
    value <- dunif$mean(input$dunif.range[1], input$dunif.range[2])
    f_str <- dunif$mean_str
    meanBox(f_str, value)
  })

  output$dunif.varianceBox <- renderValueBox({
    value <- dunif$variance(input$dunif.range[1], input$dunif.range[2])
    f_str <- dunif$variance_str
    varianceBox(f_str, value)
  })

  output$dunifPlot <- renderNvd3Chart({
    func <- dunif$func(input$dunif.p_or_c)(input$dunif.range[1], input$dunif.range[2])
    x<-seq(input$dunif.range[1], input$dunif.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

}
