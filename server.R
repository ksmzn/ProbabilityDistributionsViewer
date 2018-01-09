library(shiny)

server <- function(input, output) {
  ###########################################################################
  # Continuous probability distributions
  ###########################################################################
  callDistributionModule(norm)
  callDistributionModule(erlang)
  callDistributionModule(f)
  callDistributionModule(ncf)
  callDistributionModule(chisq)
  callDistributionModule(ncChisq)
  callDistributionModule(gamma)
  callDistributionModule(cauchy)
  callDistributionModule(exp_dist)
  callDistributionModule(lnormal)
  callDistributionModule(t_dist)
  callDistributionModule(nct)
  callDistributionModule(beta)
  callDistributionModule(ncbeta)
  callDistributionModule(unif)
  callDistributionModule(logis)
  callDistributionModule(weibull)
  ###########################################################################
  # Discrete probability distributions
  ###########################################################################
  callDistributionModule(geom)
  callDistributionModule(hyper)
  callDistributionModule(binom)
  callDistributionModule(nbinom)
  callDistributionModule(pois)
  callDistributionModule(dunif)
}
