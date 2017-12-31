library(shiny)

source_this <- function(path){
  source(file.path("./distributions", path))
}

# Continuous Distribituons
source_this("./continuous/Logistic.R")
source_this("./continuous/Normal.R")
source_this("./continuous/Log-normal.R")
source_this("./continuous/Exponential.R")
source_this("./continuous/Cauchy.R")
source_this("./continuous/Noncentral_chi-squared.R")
source_this("./continuous/Weibull.R")
source_this("./continuous/Noncentral_t.R")
source_this("./continuous/Beta.R")
source_this("./continuous/t.R")
source_this("./continuous/F.R")
source_this("./continuous/Noncentral_beta.R")
source_this("./continuous/Gamma.R")
source_this("./continuous/Uniform.R")
source_this("./continuous/Chi-squared.R")
source_this("./continuous/Noncentral_F.R")

# Discrete Distribituons
source_this("./discrete/Hypergeometric.R")
source_this("./discrete/Binomial.R")
source_this("./discrete/Poisson.R")
source_this("./discrete/Negative_binomial.R")
source_this("./discrete/Discrete_uniform.R")
source_this("./discrete/Geometric.R")
