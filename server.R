library(shiny)
library(ggplot2)

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

  output$erlangPlot <- renderPlot({
    func <- gamma.func(input$erlang.shape, input$erlang.scale, input$erlang.p_or_c)
    p <- ggplot(data.frame(x=input$erlang.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$fPlot <- renderPlot({
    func <- f.func(input$f.df1, input$f.df2, input$f.p_or_c)
    p <- ggplot(data.frame(x=input$f.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$ncfPlot <- renderPlot({
    func <- ncf.func(input$ncf.df1, input$ncf.df2, input$ncf.ncp, input$ncf.p_or_c)
    p <- ggplot(data.frame(x=input$ncf.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$chisqPlot <- renderPlot({
    func <- chisq.func(input$chisq.df, input$chisq.p_or_c)
    p <- ggplot(data.frame(x=input$chisq.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$ncChisqPlot <- renderPlot({
    func <- ncChisq.func(input$ncChisq.df, input$ncChisq.ncp, input$ncChisq.p_or_c)
    p <- ggplot(data.frame(x=input$ncChisq.range), aes(x)) +
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
    func <- t.func(input$t.df, input$t.p_or_c)
    p <- ggplot(data.frame(x=input$t.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$nctPlot <- renderPlot({
    func <- nct.func(input$nct.df, input$nct.ncp, input$nct.p_or_c)
    p <- ggplot(data.frame(x=input$nct.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$betaPlot <- renderPlot({
    func <- beta.func(input$beta.shape1, input$beta.shape2, input$beta.p_or_c)
    p <- ggplot(data.frame(x=input$beta.range), aes(x)) +
      stat_function(fun=func)
    print(p)
  })

  output$ncbetaPlot <- renderPlot({
    func <- ncbeta.func(input$ncbeta.shape1,
                        input$ncbeta.shape2,
                        input$ncbeta.ncp,
                        input$ncbeta.p_or_c)
    p <- ggplot(data.frame(x=input$ncbeta.range), aes(x)) +
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

  ####################################################
  # 離散分布
  ####################################################
  output$geomPlot <- renderPlot({
    func <- geom.func(input$geom.prob, input$geom.p_or_c)
    p <- ggplot(transform(data.frame(x=c(input$geom.range[1]:input$geom.range[2])),
                          y=func(x)), aes(x, y))
    p <- p + geom_point()
    print(p)
  })

  output$hyperPlot <- renderPlot({
    func <- hyper.func(input$hyper.m, input$hyper.n, input$hyper.k, input$hyper.p_or_c)
    p <- ggplot(transform(data.frame(x=c(input$hyper.range[1]:input$hyper.range[2])),
                          y=func(x)), aes(x, y))
    p <- p + geom_point()
    print(p)
  })

  output$binomPlot <- renderPlot({
    func <- binom.func(input$binom.size, input$binom.prob, input$binom.p_or_c)
    p <- ggplot(transform(data.frame(x=c(input$binom.range[1]:input$binom.range[2])),
                          y=func(x)), aes(x, y))
    p <- p + geom_point()
    print(p)
  })

  output$nbinomPlot <- renderPlot({
    func <- nbinom.func(input$nbinom.size, input$nbinom.prob, input$nbinom.p_or_c)
    p <- ggplot(transform(data.frame(x=c(input$nbinom.range[1]:input$nbinom.range[2])),
                          y=func(x)), aes(x, y))
    p <- p + geom_point()
    print(p)
  })

  output$poisPlot <- renderPlot({
    func <- pois.func(input$pois.lambda, input$pois.p_or_c)
    p <- ggplot(transform(data.frame(x=c(input$pois.range[1]:input$pois.range[2])),
                          y=func(x)), aes(x, y))
    p <- p + geom_point()
    print(p)
  })

  output$dunifPlot <- renderPlot({
    func <- unif.func(input$dunif.range[1], input$dunif.range[2], input$dunif.p_or_c)
    p <- ggplot(transform(data.frame(x=c(input$dunif.range[1]:input$dunif.range[2])),
                          y=func(x)), aes(x, y))
    p <- p + geom_point()
    print(p)
  })

})
