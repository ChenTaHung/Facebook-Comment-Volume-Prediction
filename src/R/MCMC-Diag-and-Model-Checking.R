source('src/R/validation-functions.R')
source('src/R/MCMC-Diagnostics.R')

library(rstan)
library(rstanarm)
library(bayesplot)
library(sjPlot)

test_all <- read_csv('Final-Project/Remote-Git/data/Dataset/testall.csv')

# NB Laplace --------------------------------------------------------------


load('data/Model-Object/Bayes/NegBinBayes_laplace_small.RDS')

prior_summary(NegBinBayes_laplace_small)

MENB_laplace_Bayes<- apply(posterior_epred(NegBinBayes_laplace_small, newdata = test_all), 2, mean)

MSE(test_all$Target_Variable, MENB_laplace_Bayes)
MAPE(test_all$Target_Variable, MENB_laplace_Bayes)
RMSE(test_all$Target_Variable, MENB_laplace_Bayes)
MAE(test_all$Target_Variable, MENB_laplace_Bayes)

MENB_laplace_Bayes_est <- data.frame(posterior_interval(NegBinBayes_laplace_small)) %>% 
  slice(1:14) %>%
  mutate(posterior_mean = fixef(NegBinBayes_laplace_small))

pp_check(NegBinBayes_laplace_small) +
  scale_x_continuous(limits = c(0, 25))

plot_model(NegBinBayes_laplace_small, type = 'diag')

plot_trace(NegBinBayes_laplace_small, params = names(fixef(NegBinBayes_laplace_small)))
plot_acf(NegBinBayes_laplace_small, params = names(fixef(NegBinBayes_laplace_small)))

# NB' ---------------------------------------------------------------------

load('data/Model-Object/Bayes/NegBinBayes_small.RDS')

prior_summary(NegBinBayesFit_small)

MENB_Bayes<- apply(posterior_epred(NegBinBayesFit_small, newdata = test_all), 2, mean)

MSE(test_all$Target_Variable, MENB_Bayes)
MAPE(test_all$Target_Variable, MENB_Bayes)
RMSE(test_all$Target_Variable, MENB_Bayes)
MAE(test_all$Target_Variable, MENB_Bayes)

MENB_Bayes_est <- data.frame(posterior_interval(NegBinBayesFit_small)) %>% 
  slice(1:14) %>%
  mutate(posterior_mean = fixef(NegBinBayesFit_small))

pp_check(NegBinBayesFit_small)+
  scale_x_continuous(limits = c(0, 25))

plot_model(NegBinBayesFit_small, type = 'diag')

plot_trace(NegBinBayesFit_small, params = names(fixef(NegBinBayesFit_small)))
plot_acf(NegBinBayesFit_small, params = names(fixef(NegBinBayesFit_small)))

# Pois --------------------------------------------------------------------

load('data/Model-Object/Bayes/PoisBayes_small.RDS')
prior_summary(PoissonBayesFit_small)
Pois_Bayes<- apply(posterior_epred(PoissonBayesFit_small, newdata = test_all), 2, mean)

MSE(test_all$Target_Variable, Pois_Bayes)
MAPE(test_all$Target_Variable, Pois_Bayes)
RMSE(test_all$Target_Variable, Pois_Bayes)
MAE(test_all$Target_Variable, Pois_Bayes)

Pois_Bayes_est <- data.frame(posterior_interval(PoissonBayesFit_small)) %>% 
  slice(1:14) %>%
  mutate(posterior_mean = fixef(PoissonBayesFit_small))

pp_check(NegBinBayesFit_small) +
  scale_x_continuous(limits = c(0, 25))

plot_model(PoissonBayesFit_small, type = 'diag')

plot_trace(PoissonBayesFit_small, params = names(fixef(PoissonBayesFit_small)))
plot_acf(PoissonBayesFit_small, params = names(fixef(PoissonBayesFit_small)))
