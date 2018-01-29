library(shiny)
library(R6)
library(purrr)

Distribution <- R6Class(
  "Distribution",
  public = list(
    dist = NULL,
    name = NULL,
    wiki = NULL,
    c_or_d = NULL,
    func = NULL,
    formula = NULL,
    x_filter = NULL,
    mean = NULL,
    mean_str = NULL,
    variance = NULL,
    variance_str = NULL,
    range = NULL,
    params = NULL,
    initialize = function(
      dist = NA,
      name = NA,
      wiki = NA,
      c_or_d = NA,
      func_p = NA,
      func_c = NA,
      formula = NA,
      x_filter = NA,
      mean = NA,
      mean_str = NA,
      variance = NA,
      variance_str = NA,
      range = NA,
      ...
    ) {
      self$dist <- dist
      self$name <- name
      self$wiki <- wiki
      self$c_or_d <- c_or_d
      self$formula <- formula
      self$mean <- mean
      self$mean_str <- mean_str
      self$variance <- variance
      self$variance_str <- variance_str
      self$range <- range
      private$set_x_filter(x_filter)
      private$set_func(func_p, func_c)
      private$set_params(dist, ...)
    }
  ),
  private = list(
    set_x_filter = function(x_filter){
      if(is.null(x_filter)){
        x_filter <- function(x, ...) x
      }
      self$x_filter <- x_filter
    },
    set_func = function(func_p, func_c){
      self$func <- function(p_or_c){
        if(p_or_c == "p"){
          return(func_p)
        } else {
          return(func_c)
        }
      }
    },
    set_params = function(dist, ...) {
      params <- list(...)
      names(params) <- sapply(params, `[[`, "name")
      self$params <- params
    }
  )
)

source_this <- function(path){
  source(file.path("./distributions", path))
}

# Continuous Distribituons
source_this("continuous/Normal.R")
source_this("continuous/Erlang.R")
source_this("continuous/F.R")
source_this("continuous/Noncentral_F.R")
source_this("continuous/Chi-squared.R")
source_this("continuous/Noncentral_chi-squared.R")
source_this("continuous/Gamma.R")
source_this("continuous/Cauchy.R")
source_this("continuous/Exponential.R")
source_this("continuous/Log-normal.R")
source_this("continuous/t.R")
source_this("continuous/Noncentral_t.R")
source_this("continuous/Beta.R")
source_this("continuous/Noncentral_beta.R")
source_this("continuous/Uniform.R")
source_this("continuous/Logistic.R")
source_this("continuous/Weibull.R")

# Discrete Distribituons
source_this("discrete/Geometric.R")
source_this("discrete/Hypergeometric.R")
source_this("discrete/Binomial.R")
source_this("discrete/Negative_binomial.R")
source_this("discrete/Poisson.R")
source_this("discrete/Discrete_uniform.R")

# All List
distributions <- list(
  # Continuous
  norm,
  erlang,
  f,
  ncf,
  chisq,
  ncChisq,
  gamma,
  cauchy,
  exp_dist,
  lnormal,
  t_dist,
  nct,
  beta,
  ncbeta,
  unif,
  logis,
  weibull,
  # Discrete
  geom,
  hyper,
  binom,
  nbinom,
  pois,
  dunif
)

dist_names <- purrr::map_chr(distributions, ~ .x$dist)
names(distributions) <- dist_names