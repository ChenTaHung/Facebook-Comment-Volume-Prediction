if(!requireNamespace('bayesplot')){
  download.packages('bayesplot')
}

library(bayesplot)
library(ggplot2)


# Trace Plot --------------------------------------------------------------


plot_trace <- function(model, params = NULL, check_separated = T) {
  if (check_separated){
    if(is.null(params)){
      trace <- mcmc_trace(as.matrix(model)) +
        scale_color_discrete()
    }
    else{
      trace <- mcmc_trace(as.matrix(model), pars = params) +
        scale_color_discrete()
    }
  }
  else{
    if(is.null(params)){
      trace <- mcmc_trace(as.matrix(model)) +
        scale_color_discrete()
    }
    else{
      trace <- mcmc_trace(as.matrix(model), pars = params) +
        scale_color_discrete()
    }
  }
  print(trace)
}

# Density Overlay Plot ----------------------------------------------------

plot_dens_overlay <- function(model, params) {
  density <- mcmc_dens_overlay(model, pars = params)
  print(density)
}


# ACF plot ----------------------------------------------------------------

plot_acf <- function(model, params = NULL, lags = 50) {
  if(is.null(params)){
    autocorr <- mcmc_acf(as.matrix(model), lags = lags)
  }
  else{
    autocorr <- mcmc_acf(as.matrix(model)[,params], lags = lags)
  }
  print(autocorr)
}