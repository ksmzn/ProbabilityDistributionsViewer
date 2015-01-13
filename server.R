library(shiny)
library(ggplot2)
library(rCharts)
# options(RCHART_LIB = 'highcharts')
shinyServer(function(input, output) {
#
#   output$homePlot <- renderPlot({
#     func <- norm.func(input$home.mean, input$home.sd, input$home.p_or_c)
#     p <- ggplot(data.frame(x=input$home.range), aes(x)) +
#       stat_function(fun=func)
#     print(p)
#   })

  output$erlangPlot <- renderNvd3Chart({
    func <- gamma.func(input$erlang.shape, input$erlang.scale, input$erlang.p_or_c)
    x<-seq(input$erlang.range[1], input$erlang.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

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

  output$cauchyPlot <- renderNvd3Chart({
    func <- cauchy.func(input$cauchy.location, input$cauchy.scale, input$cauchy.p_or_c)
    x<-seq(input$cauchy.range[1], input$cauchy.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$expPlot <- renderNvd3Chart({
    func <- exp.func(input$exp.rate, input$exp.p_or_c)
    x<-seq(input$exp.range[1], input$exp.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$normalPlot <- renderNvd3Chart({
    func <- norm.func(input$norm.mean, input$norm.sd, input$norm.p_or_c)
    x<-seq(input$norm.range[1], input$norm.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

#   output$normalPlot <- renderNvd3Chart({
#     func <- norm.func(input$norm.mean, input$norm.sd, input$norm.p_or_c)
#     p <- ggplot(data.frame(x=input$norm.range), aes(x)) +
#       stat_function(fun=func)
#     print(p)
#   })
#
  output$lnormalPlot <- renderNvd3Chart({
    func <- lnorm.func(input$lnorm.meanlog, input$lnorm.sdlog, input$lnorm.p_or_c)
    x<-seq(input$lnorm.range[1], input$lnorm.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$tPlot <- renderNvd3Chart({
    func <- t.func(input$t.df, input$t.p_or_c)
    x<-seq(input$t.range[1], input$t.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$nctPlot <- renderNvd3Chart({
    func <- nct.func(input$nct.df, input$nct.ncp, input$nct.p_or_c)
    x<-seq(input$nct.range[1], input$nct.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
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

  output$unifPlot <- renderNvd3Chart({
    func <- unif.func(input$unif.range[1], input$unif.range[2], input$unif.p_or_c)
    x<-seq(input$unif.range[1], input$unif.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$logisPlot <- renderNvd3Chart({
    func <- logis.func(input$logis.location, input$logis.scale, input$logis.p_or_c)
    x<-seq(input$logis.range[1], input$logis.range[2] ,length=1000)
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
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

  ####################################################
  # 離散分布
  ####################################################
  output$geomPlot <- renderNvd3Chart({
    func <- geom.func(input$geom.prob, input$geom.p_or_c)
#     p <- ggplot(transform(data.frame(x=c(input$geom.range[1]:input$geom.range[2])),
#                           y=func(x)), aes(x, y))
#     p <- p + geom_point()
#     print(p)
    x<-seq(input$geom.range[1], input$geom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$hyperPlot <- renderNvd3Chart({
    func <- hyper.func(input$hyper.m, input$hyper.n, input$hyper.k, input$hyper.p_or_c)
    x<-seq(input$hyper.range[1], input$hyper.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$binomPlot <- renderNvd3Chart({
    func <- binom.func(input$binom.size, input$binom.prob, input$binom.p_or_c)
    x<-seq(input$binom.range[1], input$binom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$nbinomPlot <- renderNvd3Chart({
    func <- nbinom.func(input$nbinom.size, input$nbinom.prob, input$nbinom.p_or_c)
    x<-seq(input$nbinom.range[1], input$nbinom.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$poisPlot <- renderNvd3Chart({
    func <- pois.func(input$pois.lambda, input$pois.p_or_c)
    x<-seq(input$pois.range[1], input$pois.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

  output$dunifPlot <- renderNvd3Chart({
    func <- unif.func(input$dunif.range[1], input$dunif.range[2], input$dunif.p_or_c)
    x<-seq(input$dunif.range[1], input$dunif.range[2])
    y <- eval(func(x))
    df <- data.frame(x,y)
    return(df)
  })

})
