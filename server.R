library(shiny)
library(hypergeo)
source("distributions.R")

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
  output$normal.meanBox <- renderValueBox({
    value <- input$norm.mean
    f_str <- "\\mu"
    meanBox(f_str, value)
  })

  output$normal.varianceBox <- renderValueBox({
    value <- input$norm.sd ** 2
    f_str <- "\\sigma^2"
    varianceBox(f_str, value)
  })
  # outputs for plot
  output$normalPlot <- renderNvd3Chart({
    func <- norm.func(input$norm.mean, input$norm.sd, input$norm.p_or_c)
    x<-seq(input$norm.range[1], input$norm.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })
  ###########################################################################
  # アーラン分布
  ###########################################################################
  output$erlang.meanBox <- renderValueBox({
    value <- input$erlang.shape / input$erlang.scale
    f_str <- "n/\\lambda"
    meanBox(f_str, value)
  })

  output$erlang.varianceBox <- renderValueBox({
    value <- input$erlang.shape / (input$erlang.scale ** 2)
    f_str <- "n/\\lambda^2"
    varianceBox(f_str, value)
  })

  # outputs for plot
  output$erlangPlot <- renderNvd3Chart({
    func <- gamma.func(input$erlang.shape, input$erlang.scale, input$erlang.p_or_c)
    x<-seq(input$erlang.range[1], input$erlang.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # F分布
  ###########################################################################
  output$f.meanBox <- renderValueBox({
    value <- input$f.df2 / (input$f.df2 - 2)
    f_str <- "\\frac{d_2}{d_2-2}"
    meanBox(f_str, value)
  })

  output$f.varianceBox <- renderValueBox({
    value <- (2 * input$f.df2 ** 2) * (input$f.df1 + input$f.df2 - 2) / 
      (input$f.df1 * ((input$f.df2 - 2) ** 2) * (input$f.df2 - 4))
    f_str <- "\\frac{2\\,d_2^2\\,(d_1+d_2-2)}{d_1 (d_2-2)^2 (d_2-4)}"
    varianceBox(f_str, value)
  })

  # outputs for plot
  output$fPlot <- renderNvd3Chart({
    func <- f.func(input$f.df1, input$f.df2, input$f.p_or_c)
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
    value <- (input$ncf.df2 * (input$ncf.df1 + input$ncf.ncp)) /
      (input$ncf.df1 * (input$ncf.df2 - 2))
    f_str <- "\\frac{\\nu_2(\\nu_1 + \\lambda)}{\\nu_1(\\nu_2-2)}"
    meanBox(f_str, value)
  })

  output$ncf.varianceBox <- renderValueBox({
    value <- (2 * input$ncf.df2 ** 2) * (input$ncf.df1 + input$ncf.df2 - 2) / 
      (input$ncf.df1 * ((input$ncf.df2 - 2) ** 2) * (input$ncf.df2 - 4))
    f_str <- "2\\frac{(\\nu_1+\\lambda)^2+(\\nu_1+2\\lambda)(\\nu_2-2)}{(\\nu_2-2)^2(\\nu_2-4)} \\left(\\frac{\\nu_2}{\\nu_1}\\right)^2"
    varianceBox(f_str, value)
  })

  # outputs for plot
  output$ncfPlot <- renderNvd3Chart({
    func <- ncf.func(input$ncf.df1, input$ncf.df2, input$ncf.ncp, input$ncf.p_or_c)
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
    value <- input$chisq.df
    f_str <- "k"
    meanBox(f_str, value)
  })

  output$chisq.varianceBox <- renderValueBox({
    value <- 2 * input$chisq.df
    f_str <- "2k"
    varianceBox(f_str, value)
  })

  output$chisqPlot <- renderNvd3Chart({
    func <- chisq.func(input$chisq.df, input$chisq.p_or_c)
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
    value <- input$ncChisq.df + input$ncChisq.ncp
    f_str <- "k+\\lambda"
    meanBox(f_str, value)
  })

  output$ncChisq.varianceBox <- renderValueBox({
    value <- 2 * (input$ncChisq.df + 2 * input$ncChisq.ncp)
    f_str <- "2(k+2\\lambda)"
    varianceBox(f_str, value)
  })

  output$ncChisqPlot <- renderNvd3Chart({
    func <- ncChisq.func(input$ncChisq.df, input$ncChisq.ncp, input$ncChisq.p_or_c)
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
    value <- input$gamma.shape * input$gamma.scale
    f_str <- "k\\theta"
    meanBox(f_str, value)
  })

  output$gamma.varianceBox <- renderValueBox({
    value <- input$gamma.shape * (input$gamma.scale ** 2)
    f_str <- "k\\theta^2"
    varianceBox(f_str, value)
  })

  output$gammaPlot <- renderNvd3Chart({
    func <- gamma.func(input$gamma.shape, input$gamma.scale, input$gamma.p_or_c)
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
    meanBox(NULL, NULL)
  })

  output$cauchy.varianceBox <- renderValueBox({
    varianceBox(NULL, NULL)
  })

  output$cauchyPlot <- renderNvd3Chart({
    func <- cauchy.func(input$cauchy.location, input$cauchy.scale, input$cauchy.p_or_c)
    x<-seq(input$cauchy.range[1], input$cauchy.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 指数分布
  ###########################################################################
  output$exp.meanBox <- renderValueBox({
    value <- 1 / input$exp.rate
    f_str <- "\\frac{1}{\\lambda}"
    meanBox(f_str, value)
  })

  output$exp.varianceBox <- renderValueBox({
    value <- 1 / (input$exp.rate ** 2)
    f_str <- "\\frac{1}{\\lambda^2}"
    varianceBox(f_str, value)
  })

  output$expPlot <- renderNvd3Chart({
    func <- exp.func(input$exp.rate, input$exp.p_or_c)
    x<-seq(input$exp.range[1], input$exp.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 対数正規分布
  ###########################################################################
  output$lnormal.meanBox <- renderValueBox({
    value <- exp(input$lnormal.meanlog + ((input$lnormal.sdlog ** 2) / 2))
    f_str <- "e^{\\mu+\\sigma^2/2}"
    meanBox(f_str, value)
  })

  output$lnormal.varianceBox <- renderValueBox({
    value <- exp(2 * input$lnormal.meanlog + input$lnormal.sdlog) *
      (exp(input$lnormal.sdlog ** 2) - 1)
    f_str <- "e^{2\\mu+\\sigma^2}(e^{\\sigma^2}-1)"
    varianceBox(f_str, value)
  })

  output$lnormalPlot <- renderNvd3Chart({
    func <- lnormal.func(input$lnormal.meanlog, input$lnormal.sdlog, input$lnormal.p_or_c)
    x<-seq(input$lnormal.range[1], input$lnormal.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # t分布
  ###########################################################################
  output$t.meanBox <- renderValueBox({
    value <- 0L
    f_str <- "e^{\\mu+\\sigma^2/2}"
    meanBox(f_str, value)
  })

  output$t.varianceBox <- renderValueBox({
    if (input$t.df <= 2){
      value <- Inf
    } else {
      value <- input$t.df / (input$t.df - 2)
    }
    f_str <- "\\frac{\\nu}{\\nu-2}"
    varianceBox(f_str, value)
  })

  output$tPlot <- renderNvd3Chart({
    func <- t.func(input$t.df, input$t.p_or_c)
    x<-seq(input$t.range[1], input$t.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 非心t分布
  ###########################################################################
  output$nct.meanBox <- renderValueBox({
    if(input$nct.df <= 1){
      value <- NULL
      f_str <- NULL
    } else {
      value <- input$nct.ncp * sqrt(input$nct.df / 2) * 
        gamma((input$nct.df - 1) / 2) / gamma(input$nct.df / 2)
      f_str <- "\\mu\\sqrt{\\frac{\\nu}{2}}\\frac{\\Gamma((\\nu-1)/2)}{\\Gamma(\\nu/2)}"
    }
    meanBox(f_str, value)
  })

  output$nct.varianceBox <- renderValueBox({
    if (input$nct.df <= 2){
      value <- NULL
      f_str <- NULL
    } else {
      value <- (input$nct.df * (1 + input$nct.ncp ** 2)) / (input$nct.df - 2) -
        ((input$nct.ncp ** 2) * input$nct.df / 2) * 
        ((gamma((input$nct.df - 1) / 2) / gamma(input$nct.df / 2)) ** 2)
      f_str <- "\\frac{\\nu(1+\\mu^2)}{\\nu-2}-\\frac{\\mu^2\\nu}{2}\\left(\\frac{\\Gamma((\\nu-1)/2)}{\\Gamma(\\nu/2)}\\right)^2"
    }
    varianceBox(f_str, value)
  })

  output$nctPlot <- renderNvd3Chart({
    func <- nct.func(input$nct.df, input$nct.ncp, input$nct.p_or_c)
    x<-seq(input$nct.range[1], input$nct.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # ベータ分布
  ###########################################################################
  output$beta.meanBox <- renderValueBox({
    value <- input$beta.shape1 / (input$beta.shape1 + input$beta.shape2)
    f_str <- "\\frac{\\alpha}{\\alpha+\\beta}"
    meanBox(f_str, value)
  })

  output$beta.varianceBox <- renderValueBox({
    value <- (input$beta.shape1 * input$beta.shape2) /
      (((input$beta.shape1 + input$beta.shape2) ** 2) *
      (input$beta.shape1 + input$beta.shape1 + 1))
    f_str <- "\\frac{\\alpha\\beta}{(\\alpha+\\beta)^2 (\\alpha+\\beta+1)}"
    varianceBox(f_str, value)
  })

  output$betaPlot <- renderNvd3Chart({
    func <- beta.func(input$beta.shape1, input$beta.shape2, input$beta.p_or_c)
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
    ncp <- input$ncbeta.ncp
    shape1 <- input$ncbeta.shape1
    shape2 <- input$ncbeta.shape2
    value <- exp(- ncp / 2) * shape1 *
      genhypergeo(U = c(shape1 + 1, shape1 + shape2),
                  L = c(shape1, 1 + shape1 + shape2),
                  z = ncp/2) /
      (shape1 + shape2)
    f_str <- "e^{-\\frac{\\lambda}{2}}
        \\frac{\\alpha}{\\alpha+\\beta}
        {}_2F_2\\left(\\alpha+\\beta,\\alpha+1;\\alpha,\\alpha+\\beta+1;\\frac{\\lambda}{2}
        \\right)"
    meanBox(f_str, value)
  })

  output$ncbeta.varianceBox <- renderValueBox({
    ncp <- input$ncbeta.ncp
    shape1 <- input$ncbeta.shape1
    shape2 <- input$ncbeta.shape2
    value.mean <- exp(- ncp / 2) * shape1 *
      genhypergeo(U = c(shape1 + 1, shape1 + shape2),
                  L = c(shape1, 1 + shape1 + shape2),
                  z = ncp/2) /
      (shape1 + shape2)
    value <- exp(- ncp / 2) * shape1 * (shape1 + 1) *
      genhypergeo(U = c(shape1 + 1, shape1 + shape2),
                  L = c(shape1, 1 + shape1 + shape2),
                  z = ncp/2) /
      ((shape1 + shape2) *
       (shape1 + shape2 + 1)) -
      value.mean ** 2
    f_str <- "e^{-\\frac{\\lambda}{2}}
        \\frac{\\alpha (\\alpha+1)}{(\\alpha+\\beta)(\\alpha+\\beta+1)}
        {}_2F_2\\left(\\alpha+\\beta,\\alpha+2;\\alpha,\\alpha+\\beta+2
        ;\\frac{\\lambda}{2}\\right) - \\mu^2"
    varianceBox(f_str, value)
  })

  output$ncbetaPlot <- renderNvd3Chart({
    func <- ncbeta.func(input$ncbeta.shape1,
                        input$ncbeta.shape2,
                        input$ncbeta.ncp,
                        input$ncbeta.p_or_c)
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
    value <- (input$unif.range[1] + input$unif.range[2]) / 2
    f_str <- "\\frac{1}{2}(a+b)"
    meanBox(f_str, value)
  })

  output$unif.varianceBox <- renderValueBox({
    value <- ((input$unif.range[2] - input$unif.range[1]) ** 2) / 12
    f_str <- "\\frac{1}{12}(b-a)^2"
    varianceBox(f_str, value)
  })

  output$unifPlot <- renderNvd3Chart({
    func <- unif.func(input$unif.range[1], input$unif.range[2], input$unif.p_or_c)
    x<-seq(input$unif.range[1], input$unif.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # ロジスティック分布
  ###########################################################################
  output$logis.meanBox <- renderValueBox({
    value <- input$logis.location
    f_str <- "\\mu"
    meanBox(f_str, value)
  })

  output$logis.varianceBox <- renderValueBox({
    value <- ((input$logis.scale * pi) ** 2) / 3
    f_str <- "\\frac{\\pi^2}{3} s^2"
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
    value <- input$weibull.scale * gamma(1 + 1 / input$weibull.shape)
    f_str <- "\\eta \\Gamma(1+1/m)"
    meanBox(f_str, value)
  })

  output$weibull.varianceBox <- renderValueBox({
    value <- (input$weibull.scale ** 2) *
      (gamma(1 + 2 / input$weibull.shape) - gamma(1 + 1 / input$weibull.shape) ** 2)
    f_str <- "\\eta^2\\left[\\Gamma\\left(1+\\frac{2}{m}\\right) - \\left(\\Gamma\\left(1+\\frac{1}{m}\\right)\\right)^2\\right]"
    varianceBox(f_str, value)
  })

  output$weibullPlot <- renderNvd3Chart({
    func <- weibull.func(input$weibull.shape, input$weibull.scale, input$weibull.p_or_c)
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
    value <- (1 - input$geom.prob) / input$geom.prob
    f_str <- "\\frac{1-p}{p}"
    meanBox(f_str, value)
  })

  output$geom.varianceBox <- renderValueBox({
    value <- (1 - input$geom.prob) / input$geom.prob ** 2
    f_str <- "\\frac{1-p}{p^2}"
    varianceBox(f_str, value)
  })

  output$geomPlot <- renderNvd3Chart({
    func <- geom.func(input$geom.prob, input$geom.p_or_c)
    x<-seq(input$geom.range[1], input$geom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 超幾何分布
  ###########################################################################
  output$hyper.meanBox <- renderValueBox({
    value <- input$hyper.k * input$hyper.m / (input$hyper.m + input$hyper.n)
    f_str <- "\\frac{km}{m+n}"
    meanBox(f_str, value)
  })

  output$hyper.varianceBox <- renderValueBox({
    k <- input$hyper.k
    m <- input$hyper.m
    n <- input$hyper.n
    value <- k * m * n * (m + n - k) / (((m + n) ** 2) * (m + n - 1))
    f_str <- "\\frac{kmn(m+n-k)}{(m+n)^2 (m+n-1)}"
    varianceBox(f_str, value)
  })

  output$hyperPlot <- renderNvd3Chart({
    func <- hyper.func(input$hyper.m, input$hyper.n, input$hyper.k, input$hyper.p_or_c)
    x<-seq(input$hyper.range[1], input$hyper.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 二項分布
  ###########################################################################
  output$binom.meanBox <- renderValueBox({
    value <- input$binom.size * input$binom.prob
    f_str <- "np"
    meanBox(f_str, value)
  })

  output$binom.varianceBox <- renderValueBox({
    value <- input$binom.size * input$binom.prob * (1 - input$binom.prob)
    f_str <- "np(1-p)"
    varianceBox(f_str, value)
  })

  output$binomPlot <- renderNvd3Chart({
    func <- binom.func(input$binom.size, input$binom.prob, input$binom.p_or_c)
    x<-seq(input$binom.range[1], input$binom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 負の二項分布
  ###########################################################################
  output$nbinom.meanBox <- renderValueBox({
    value <- input$nbinom.size / input$nbinom.prob
    f_str <- "\\frac{r}{p}"
    meanBox(f_str, value)
  })

  output$nbinom.varianceBox <- renderValueBox({
    value <- input$nbinom.size * (1 - input$nbinom.prob) / (input$nbinom.prob ** 2)
    f_str <- "\\frac{r(1-p)}{p^2}"
    varianceBox(f_str, value)
  })

  output$nbinomPlot <- renderNvd3Chart({
    func <- nbinom.func(input$nbinom.size, input$nbinom.prob, input$nbinom.p_or_c)
    x<-seq(input$nbinom.range[1], input$nbinom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # ポアソン分布
  ###########################################################################
  output$pois.meanBox <- renderValueBox({
    value <- input$pois.lambda
    f_str <- "\\lambda"
    meanBox(f_str, value)
  })

  output$pois.varianceBox <- renderValueBox({
    value <- input$pois.lambda
    f_str <- "\\lambda"
    varianceBox(f_str, value)
  })

  output$poisPlot <- renderNvd3Chart({
    func <- pois.func(input$pois.lambda, input$pois.p_or_c)
    x<-seq(input$pois.range[1], input$pois.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ###########################################################################
  # 連続一様分布
  ###########################################################################
  output$dunif.meanBox <- renderValueBox({
    value <- (input$dunif.range[1] + input$dunif.range[2]) / 2
    f_str <- "\\frac{a+b}{2}"
    meanBox(f_str, value)
  })

  output$dunif.varianceBox <- renderValueBox({
    value <- (((input$dunif.range[2] - input$dunif.range[1] + 1) ** 2) - 1) / 12
    f_str <- "\\frac{(b-a+1)^2 -1}{12}"
    varianceBox(f_str, value)
  })

  output$dunifPlot <- renderNvd3Chart({
    func <- unif.func(input$dunif.range[1], input$dunif.range[2],
      input$dunif.p_or_c)
    x<-seq(input$dunif.range[1], input$dunif.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

}
