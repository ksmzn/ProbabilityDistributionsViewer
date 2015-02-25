library(shiny)
library(hypergeo)
source("distributions.R")

boxcolor <- "blue"
mean.icon <- icon("star", lib = "glyphicon")
variance.icon <- icon("resize-horizontal", lib = "glyphicon")
server <- function(input, output) {
  ################################################################################
  # 正規分布
  ###########################################################################
  output$normal.meanBox <- renderValueBox({
    valueBox(
      withMathJax(paste0("\\(\\mu\\!=\\!", input$norm.mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
  })
  output$normal.varianceBox <- renderValueBox({
    var <- input$norm.sd ** 2
    valueBox(
      withMathJax(paste0("\\(\\sigma^2\\!=\\!", var, "\\)")),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
  })
  # outputs for plot
  output$normalPlot <- renderNvd3Chart({
    func <- norm.func(input$norm.mean, input$norm.sd, input$norm.p_or_c)
    x<-seq(input$norm.range[1], input$norm.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })
  ################################################################################
  # アーラン分布
  ###########################################################################
  output$erlang.meanBox <- renderValueBox({
    mean <- input$erlang.shape / input$erlang.scale
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(n/\\lambda\\!=\\!", mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$erlang.varianceBox <- renderValueBox({
    var <- input$erlang.shape / (input$erlang.scale ** 2)
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(n/\\lambda^2\\!=\\!", var, "\\)")),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  # outputs for plot
  output$erlangPlot <- renderNvd3Chart({
    func <- gamma.func(input$erlang.shape, input$erlang.scale, input$erlang.p_or_c)
    x<-seq(input$erlang.range[1], input$erlang.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # F分布
  ###########################################################################
  output$f.meanBox <- renderValueBox({
    mean <- input$f.df2 / (input$f.df2 - 2)
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{d_2}{d_2-2}\\!=\\!", mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$f.varianceBox <- renderValueBox({
    var <- (2 * input$f.df2 ** 2) * (input$f.df1 + input$f.df2 - 2) / 
      (input$f.df1 * ((input$f.df2 - 2) ** 2) * (input$f.df2 - 4))
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{2\\,d_2^2\\,(d_1+d_2-2)}{d_1 (d_2-2)^2 (d_2-4)}\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
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

  ################################################################################
  # 非心F分布
  ###########################################################################
  output$ncf.meanBox <- renderValueBox({
    mean <- (input$ncf.df2 * (input$ncf.df1 + input$ncf.ncp)) /
      (input$ncf.df1 * (input$ncf.df2 - 2))
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{\\nu_2(\\nu_1 + \\lambda)}{\\nu_1(\\nu_2-2)}\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$ncf.varianceBox <- renderValueBox({
    var <- (2 * input$ncf.df2 ** 2) * (input$ncf.df1 + input$ncf.df2 - 2) / 
      (input$ncf.df1 * ((input$ncf.df2 - 2) ** 2) * (input$ncf.df2 - 4))
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(2\\frac{(\\nu_1+\\lambda)^2+(\\nu_1+2\\lambda)(\\nu_2-2)}{(\\nu_2-2)^2(\\nu_2-4)}
        \\left(\\frac{\\nu_2}{\\nu_1}\\right)^2\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
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
  ################################################################################
  # カイ二乗分布
  ###########################################################################
  output$chisq.meanBox <- renderValueBox({
    mean <- input$chisq.df
    box <- valueBox(
      withMathJax(paste0("\\(k\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$chisq.varianceBox <- renderValueBox({
    var <- 2 * input$chisq.df
    box <- valueBox(
      withMathJax(paste0(
        "\\(2k\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
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

  ################################################################################
  # 非心カイ二乗分布
  ###########################################################################
  output$ncChisq.meanBox <- renderValueBox({
    mean <- input$ncChisq.df + input$ncChisq.ncp
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(k+\\lambda\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$ncChisq.varianceBox <- renderValueBox({
    var <- 2 * (input$ncChisq.df + 2 * input$ncChisq.ncp)
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(2(k+2\\lambda)\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
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

  ################################################################################
  # ガンマ分布
  ###########################################################################
  output$gamma.meanBox <- renderValueBox({
    mean <- input$gamma.shape * input$gamma.scale
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(k\\theta\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$gamma.varianceBox <- renderValueBox({
    var <- input$gamma.shape * (input$gamma.scale ** 2)
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(k\\theta^2\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
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

  ################################################################################
  # コーシー分布
  ###########################################################################
  output$cauchy.meanBox <- renderValueBox({
    box <- valueBox(
      "定義されない",
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$cauchy.varianceBox <- renderValueBox({
    box <- valueBox(
      "定義されない",
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$cauchyPlot <- renderNvd3Chart({
    func <- cauchy.func(input$cauchy.location, input$cauchy.scale, input$cauchy.p_or_c)
    x<-seq(input$cauchy.range[1], input$cauchy.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # 指数分布
  ###########################################################################
  output$exp.meanBox <- renderValueBox({
    mean <- 1 / input$exp.rate
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{1}{\\lambda}\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$exp.varianceBox <- renderValueBox({
    var <- 1 / (input$exp.rate ** 2)
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{1}{\\lambda^2}\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$expPlot <- renderNvd3Chart({
    func <- exp.func(input$exp.rate, input$exp.p_or_c)
    x<-seq(input$exp.range[1], input$exp.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # 対数正規分布
  ###########################################################################
  output$lnormal.meanBox <- renderValueBox({
    mean <- exp(input$lnormal.meanlog + ((input$lnormal.sdlog ** 2) / 2))
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(e^{\\mu+\\sigma^2/2}\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$lnormal.varianceBox <- renderValueBox({
    var <- exp(2 * input$lnormal.meanlog + input$lnormal.sdlog) *
      (exp(input$lnormal.sdlog ** 2) - 1)
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(e^{2\\mu+\\sigma^2}(e^{\\sigma^2}-1)\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$lnormalPlot <- renderNvd3Chart({
    func <- lnormal.func(input$lnormal.meanlog, input$lnormal.sdlog, input$lnormal.p_or_c)
    x<-seq(input$lnormal.range[1], input$lnormal.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # t分布
  ###########################################################################
  output$t.meanBox <- renderValueBox({
    mean <- 0
    box <- valueBox(
      withMathJax("\\(e^{\\mu+\\sigma^2/2}\\!=\\!0\\)"),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$t.varianceBox <- renderValueBox({
    if (input$t.df <= 2){
      var <- Inf
    } else {
      var <- input$t.df / (input$t.df - 2)
      var <- round(var, digits = 3)
    }
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{\\nu}{\\nu-2}\\!=\\!",
        var, "\\)")
      ),
      "分散 (自由度が2以下のときは∞)",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$tPlot <- renderNvd3Chart({
    func <- t.func(input$t.df, input$t.p_or_c)
    x<-seq(input$t.range[1], input$t.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # 非心t分布
  ###########################################################################
  output$nct.meanBox <- renderValueBox({
    if (input$nct.df <= 1) {
      meanformula <- "定義されない"
    } else {
      mean <- input$nct.ncp * sqrt(input$nct.df / 2) * 
        gamma((input$nct.df - 1) / 2) / gamma(input$nct.df / 2)
      mean <- round(mean, digits = 3)
      meanformula <- withMathJax(paste0(
        "\\(\\mu\\sqrt{\\frac{\\nu}{2}}\\frac{\\Gamma((\\nu-1)/2)}{\\Gamma(\\nu/2)}
        \\!=\\!",
        mean, "\\)"))
    }
    box <- valueBox(
      meanformula,
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$nct.varianceBox <- renderValueBox({
    if (input$nct.df <= 2){
      varformula <- "定義されない"
    } else {
      var <- (input$nct.df * (1 + input$nct.ncp ** 2)) / (input$nct.df - 2) -
        ((input$nct.ncp ** 2) * input$nct.df / 2) * 
        ((gamma((input$nct.df - 1) / 2) / gamma(input$nct.df / 2)) ** 2)
      var <- round(var, digits = 3)
      varformula <- withMathJax(paste0(
        "\\(\\frac{\\nu(1+\\mu^2)}{\\nu-2}
        -\\frac{\\mu^2\\nu}{2}
        \\left(\\frac{\\Gamma((\\nu-1)/2)}{\\Gamma(\\nu/2)}\\right)^2\\!=\\!",
        var, "\\)")
      )
    }
    box <- valueBox(
      varformula,
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$nctPlot <- renderNvd3Chart({
    func <- nct.func(input$nct.df, input$nct.ncp, input$nct.p_or_c)
    x<-seq(input$nct.range[1], input$nct.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # ベータ分布
  ###########################################################################
  output$beta.meanBox <- renderValueBox({
    mean <- input$beta.shape1 / (input$beta.shape1 + input$beta.shape2)
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{\\alpha}{\\alpha+\\beta}\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$beta.varianceBox <- renderValueBox({
    var <- (input$beta.shape1 * input$beta.shape2) /
      (((input$beta.shape1 + input$beta.shape2) ** 2) *
      (input$beta.shape1 + input$beta.shape1 + 1))
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(\\frac{\\alpha\\beta}{(\\alpha+\\beta)^2 (\\alpha+\\beta+1)}\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
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
  ################################################################################
  # 非心ベータ分布
  ###########################################################################
  output$ncbeta.meanBox <- renderValueBox({
    mean <- exp(- input$ncbeta.ncp / 2) * input$ncbeta.shape1 *
      genhypergeo(U = c(input$ncbeta.shape1 + 1, input$ncbeta.shape1 + input$ncbeta.shape2),
                  L = c(input$ncbeta.shape1, 1 + input$ncbeta.shape1 + input$ncbeta.shape2),
                  z = input$ncbeta.ncp/2) /
      (input$ncbeta.shape1 + input$ncbeta.shape2)
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(
        e^{-\\frac{\\lambda}{2}}
        \\frac{\\alpha}{\\alpha+\\beta}
        {}_2F_2\\left(\\alpha+\\beta,\\alpha+1;\\alpha,\\alpha+\\beta+1;\\frac{\\lambda}{2}
        \\right)
        \\!=\\!",
        mean, "\\)")),
      "期待値μ",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$ncbeta.varianceBox <- renderValueBox({
    mean <- exp(- input$ncbeta.ncp / 2) * input$ncbeta.shape1 *
      genhypergeo(U = c(input$ncbeta.shape1 + 1, input$ncbeta.shape1 + input$ncbeta.shape2),
                  L = c(input$ncbeta.shape1, 1 + input$ncbeta.shape1 + input$ncbeta.shape2),
                  z = input$ncbeta.ncp/2) /
      (input$ncbeta.shape1 + input$ncbeta.shape2)
    var <- exp(- input$ncbeta.ncp / 2) * input$ncbeta.shape1 * (input$ncbeta.shape1 + 1) *
      genhypergeo(U = c(input$ncbeta.shape1 + 1, input$ncbeta.shape1 + input$ncbeta.shape2),
                  L = c(input$ncbeta.shape1, 1 + input$ncbeta.shape1 + input$ncbeta.shape2),
                  z = input$ncbeta.ncp/2) /
      ((input$ncbeta.shape1 + input$ncbeta.shape2) *
       (input$ncbeta.shape1 + input$ncbeta.shape2 + 1)) -
      mean ** 2
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(
        e^{-\\frac{\\lambda}{2}}
        \\frac{\\alpha (\\alpha+1)}{(\\alpha+\\beta)(\\alpha+\\beta+1)}
        {}_2F_2\\left(\\alpha+\\beta,\\alpha+2;\\alpha,\\alpha+\\beta+2
        ;\\frac{\\lambda}{2}\\right) - \\mu^2
        \\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })


  output$ncbetaPlot <- renderNvd3Chart({
    func <- ncbeta.func(input$ncbeta.shape1,
                        input$ncbeta.shape2,
                        input$ncbeta.ncp,
                        input$ncbeta.p_or_c)
    x<-seq(input$ncbeta.range[1], input$ncbeta.range[2] ,length=1000)
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

  ################################################################################
  # 連続一様分布
  ###########################################################################
  output$unif.meanBox <- renderValueBox({
    mean <- (input$unif.range[1] + input$unif.range[2]) / 2
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{1}{2}(\\alpha+\\beta)\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$unif.varianceBox <- renderValueBox({
    var <- ((input$unif.range[2] - input$unif.range[1]) ** 2) / 12
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(\\frac{1}{12}(\\beta-\\alpha)^2\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$unifPlot <- renderNvd3Chart({
    func <- unif.func(input$unif.range[1], input$unif.range[2], input$unif.p_or_c)
    x<-seq(input$unif.range[1], input$unif.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # ロジスティック分布
  ###########################################################################
  output$logis.meanBox <- renderValueBox({
    mean <- input$logis.location
    box <- valueBox(
      withMathJax(paste0("\\(\\mu\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$logis.varianceBox <- renderValueBox({
    var <- ((input$logis.scale * pi) ** 2) / 3
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(
        \\frac{\\pi^2}{3} s^2
        \\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$logisPlot <- renderNvd3Chart({
    func <- logis.func(input$logis.location, input$logis.scale, input$logis.p_or_c)
    x<-seq(input$logis.range[1], input$logis.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })
  ################################################################################
  # ロジスティック分布
  ###########################################################################
  output$weibull.meanBox <- renderValueBox({
    mean <- input$weibull.scale * gamma(1 + 1 / input$weibull.shape)
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\eta \\Gamma(1+1/m)\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$weibull.varianceBox <- renderValueBox({
    var <- (input$weibull.scale ** 2) *
      (gamma(1 + 2 / input$weibull.shape) - gamma(1 + 1 / input$weibull.shape) ** 2)
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(
        \\eta^2\\left[\\Gamma\\left(1+\\frac{2}{m}\\right) -
          \\left(\\Gamma\\left(1+\\frac{1}{m}\\right)\\right)^2\\right]\\,
        \\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
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

  ################################################################################
  # 幾何分布
  ###########################################################################
  output$geom.meanBox <- renderValueBox({
    mean <- (1 - input$geom.prob) / input$geom.prob
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{1-p}{p}\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$geom.varianceBox <- renderValueBox({
    var <- (1 - input$geom.prob) / input$geom.prob ** 2
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(\\frac{1-p}{p^2}\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$geomPlot <- renderNvd3Chart({
    func <- geom.func(input$geom.prob, input$geom.p_or_c)
    x<-seq(input$geom.range[1], input$geom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # 超幾何分布
  ###########################################################################
  output$hyper.meanBox <- renderValueBox({
    mean <- input$hyper.k * input$hyper.m / (input$hyper.m + input$hyper.n)
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{km}{m+n}\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$hyper.varianceBox <- renderValueBox({
    var <- input$hyper.k * input$hyper.m * input$hyper.n *
      (input$hyper.m + input$hyper.n - input$hyper.k) /
      (((input$hyper.m + input$hyper.n) ** 2) * (input$hyper.m + input$hyper.n - 1))
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(\\frac{kmn(m+n-k)}{(m+n)^2 (m+n-1)}\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$hyperPlot <- renderNvd3Chart({
    func <- hyper.func(input$hyper.m, input$hyper.n, input$hyper.k, input$hyper.p_or_c)
    x<-seq(input$hyper.range[1], input$hyper.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # 二項分布
  ###########################################################################
  output$binom.meanBox <- renderValueBox({
    mean <- input$binom.size * input$binom.prob
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(np\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$binom.varianceBox <- renderValueBox({
    var <- input$binom.size * input$binom.prob * (1 - input$binom.prob)
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(np(1-p)\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$binomPlot <- renderNvd3Chart({
    func <- binom.func(input$binom.size, input$binom.prob, input$binom.p_or_c)
    x<-seq(input$binom.range[1], input$binom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # 負の二項分布
  ###########################################################################
  output$nbinom.meanBox <- renderValueBox({
    mean <- input$nbinom.size / input$nbinom.prob
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0("\\(\\frac{r}{p}\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$nbinom.varianceBox <- renderValueBox({
    var <- input$nbinom.size * (1 - input$nbinom.prob) / (input$nbinom.prob ** 2)
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(\\frac{r(1-p)}{p^2}\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$nbinomPlot <- renderNvd3Chart({
    func <- nbinom.func(input$nbinom.size, input$nbinom.prob, input$nbinom.p_or_c)
    x<-seq(input$nbinom.range[1], input$nbinom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # ポアソン分布
  ###########################################################################
  output$pois.meanBox <- renderValueBox({
    mean <- input$pois.lambda
    box <- valueBox(
      withMathJax(paste0("\\(\\lambda\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$pois.varianceBox <- renderValueBox({
    var <- input$pois.lambda
    box <- valueBox(
      withMathJax(paste0(
        "\\(\\lambda\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
  })

  output$poisPlot <- renderNvd3Chart({
    func <- pois.func(input$pois.lambda, input$pois.p_or_c)
    x<-seq(input$pois.range[1], input$pois.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  ################################################################################
  # 連続一様分布
  ###########################################################################
  output$dunif.meanBox <- renderValueBox({
    mean <- (input$dunif.range[1] + input$dunif.range[2]) / 2
    mean <- round(mean, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(\\frac{a+b}{2}\\!=\\!",
        mean, "\\)")),
      "期待値",
      icon = mean.icon,
      color = boxcolor
    )
    return(box)
  })

  output$dunif.varianceBox <- renderValueBox({
    var <- (((input$dunif.range[2] - input$dunif.range[1] + 1) ** 2) - 1) / 12
    var <- round(var, digits = 3)
    box <- valueBox(
      withMathJax(paste0(
        "\\(\\frac{(b-a+1)^2 -1}{12}\\!=\\!",
        var, "\\)")
      ),
      "分散",
      icon = variance.icon,
      color = boxcolor
    )
    return(box)
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
