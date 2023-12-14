library(rstan)
library(rstanarm)
library(bayesplot)


# NB Laplace --------------------------------------------------------------


load('data/Model-Object/Bayes/NegBinBayes_laplace_small.RDS')

prior_summary(NegBinBayes_laplace_small)


# NB' ---------------------------------------------------------------------

load('data/Model-Object/Bayes/NegBinBayes_small.RDS')

prior_summary(NegBinBayesFit_small)

# Pois --------------------------------------------------------------------


