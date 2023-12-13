source('src/R/validation-functions.R')

test_all <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/testall.csv')


# Load Feature Select ME ZI NB --------------------------------------------

load('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/FeatureSelected_MixEffZeroInfNegBinFreqFit.RDS')

# Load mixed effect negative binomial Bayes fit -------------

load('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/MixEffNegBinBayesFit.RDS')


# Load mixed effect zero inflation negative binomial freq fit -------------

load('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/MixEffZeroInfNegBinFreqFit.RDS')


# Load ZI NB freq fit -----------------------------------------------------

load('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/zInfNegBinFreqFit_noMixEff.RDS')

# Load QuasiPoisson fit ---------------------------------------------------

load('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/quasiPoisFreqFit.RDS')

# Fitted Model ------------------------------------------------------------


# Feature Selected Mixed Effect Zero Inflation NB -------------------------

MSE(fv5_train$Target_Variable, fitted(MixEffZeroInfNegBinFit_FS))
MAPE(fv5_train$Target_Variable, fitted(MixEffZeroInfNegBinFit_FS))
RMSE(fv5_train$Target_Variable, fitted(MixEffZeroInfNegBinFit_FS))
MAE(fv5_train$Target_Variable, fitted(MixEffZeroInfNegBinFit_FS))

plot_residuals(fitted = fitted(MixEffZeroInfNegBinFit_FS), residvalue = resid(MixEffZeroInfNegBinFit_FS))
plot_qq(residvalue = resid(MixEffZeroInfNegBinFit_FS))
plot_binned_residuals(fitted = fitted(MixEffZeroInfNegBinFit_FS), residvalue = resid(MixEffZeroInfNegBinFit_FS))

# Mixed Effect zero Inflation Negative Binomial ---------------------------

MSE(fv5_train$Target_Variable, fitted(MixEffZeroInfNegBinFit))
MAPE(fv5_train$Target_Variable, fitted(MixEffZeroInfNegBinFit))
RMSE(fv5_train$Target_Variable, fitted(MixEffZeroInfNegBinFit))
MAE(fv5_train$Target_Variable, fitted(MixEffZeroInfNegBinFit))

plot_residuals(fitted = fitted(MixEffZeroInfNegBinFit), residvalue = resid(MixEffZeroInfNegBinFit))
plot_qq(residvalue = resid(MixEffZeroInfNegBinFit))
plot_binned_residuals(fitted = fitted(MixEffZeroInfNegBinFit), residvalue = resid(MixEffZeroInfNegBinFit))


# Mixed Effect Negative Binomial Bayes ------------------------------------

MENB_bayes_fitted <- posterior_epred(NegBinBayesFit, newdata = fv5_train)
length(apply(MENB_bayes_fitted, 1, mean))
MENB_bayes_fitted_mean <- apply(MENB_bayes_fitted, 2, mean)

MSE(fv5_train$Target_Variable, MENB_bayes_fitted_mean)
MAPE(fv5_train$Target_Variable, MENB_bayes_fitted_mean)
RMSE(fv5_train$Target_Variable, MENB_bayes_fitted_mean)
MAE(fv5_train$Target_Variable, MENB_bayes_fitted_mean)

# Quasi-Poisson -----------------------------------------------------------

MSE(fv5_train$Target_Variable, fitted(quasiPoisFit))
MAPE(fv5_train$Target_Variable, fitted(quasiPoisFit))
RMSE(fv5_train$Target_Variable, fitted(quasiPoisFit))
MAE(fv5_train$Target_Variable, fitted(quasiPoisFit))

plot_residuals(fitted = fitted(quasiPoisFit), residvalue = resid(quasiPoisFit))
plot_qq(residvalue = resid(quasiPoisFit))
plot_binned_residuals(fitted = fitted(quasiPoisFit), residvalue = resid(quasiPoisFit))

# Zero Inflation NB - no mixed eff ----------------------------------------

MSE(fv5_train$Target_Variable, fitted(zInfNegBinFit_noMixEff))
MAPE(fv5_train$Target_Variable, fitted(zInfNegBinFit_noMixEff))
RMSE(fv5_train$Target_Variable, fitted(zInfNegBinFit_noMixEff))
MAE(fv5_train$Target_Variable, fitted(zInfNegBinFit_noMixEff))

plot_residuals(fitted = fitted(zInfNegBinFit_noMixEff), residvalue = resid(zInfNegBinFit_noMixEff))
plot_qq(residvalue = resid(zInfNegBinFit_noMixEff))
plot_binned_residuals(fitted = fitted(zInfNegBinFit_noMixEff), residvalue = resid(zInfNegBinFit_noMixEff))

# Null Model - NB ---------------------------------------------------------

MSE(fv5_train$Target_Variable, fitted(zInfNegBinNullModel))
MAPE(fv5_train$Target_Variable, fitted(zInfNegBinNullModel))
RMSE(fv5_train$Target_Variable, fitted(zInfNegBinNullModel))
MAE(fv5_train$Target_Variable, fitted(zInfNegBinNullModel))

plot_residuals(fitted = fitted(zInfNegBinNullModel), residvalue = resid(zInfNegBinNullModel))
plot_qq(residvalue = resid(zInfNegBinNullModel))
plot_binned_residuals(fitted = fitted(zInfNegBinNullModel), residvalue = resid(zInfNegBinNullModel))

# Prediction --------------------------------------------------------------


# Feature Selection ME ZI MB ----------------------------------------------

MEZINB_FS_Pred <- as.integer(predict(MixEffZeroInfNegBinFit_FS, newdata = test_all, type = 'response'))
MSE(test_all$Target_Variable, MEZINB_FS_Pred)
MAPE(test_all$Target_Variable, MEZINB_FS_Pred)
RMSE(test_all$Target_Variable, MEZINB_FS_Pred)
MAE(test_all$Target_Variable, MEZINB_FS_Pred)

# Mixed Effect zero Inflation Negative Binomial ---------------------------

MEZINB_Pred <- as.integer(predict(MixEffZeroInfNegBinFit, newdata = test_all, type = 'response'))

MSE(test_all$Target_Variable, MEZINB_Pred)
MAPE(test_all$Target_Variable, MEZINB_Pred)
RMSE(test_all$Target_Variable, MEZINB_Pred)
MAE(test_all$Target_Variable, MEZINB_Pred)

# Mixed Effect Negative Binomial Bayes ------------------------------------

MENB_Bayes<- apply(posterior_epred(NegBinBayesFit, newdata = test_all), 2, mean)

MSE(test_all$Target_Variable, MENB_Bayes)
MAPE(test_all$Target_Variable, MENB_Bayes)
RMSE(test_all$Target_Variable, MENB_Bayes)
MAE(test_all$Target_Variable, MENB_Bayes)

# Quasi-Poisson -----------------------------------------------------------

quasi_Pred <- as.integer(predict(quasiPoisFit, newdata = test_all, type = 'response'))

MSE(test_all$Target_Variable, quasi_Pred)
MAPE(test_all$Target_Variable, quasi_Pred)
RMSE(test_all$Target_Variable, quasi_Pred)
MAE(test_all$Target_Variable, quasi_Pred)


# Zero-Inflation model (no Mixed effect) ----------------------------------

ZINBM_noME_Pred <- as.integer(predict(zInfNegBinFit_noMixEff, newdata = test_all, type = 'response'))

MSE(test_all$Target_Variable, ZINBM_noME_Pred)
MAPE(test_all$Target_Variable, ZINBM_noME_Pred)
RMSE(test_all$Target_Variable, ZINBM_noME_Pred)
MAE(test_all$Target_Variable, ZINBM_noME_Pred)

# Null - NBM --------------------------------------------------------------

null_pred <- as.integer(predict(zInfNegBinNullModel, newdata = test_all, type = 'response'))
MSE(test_all$Target_Variable, null_pred)
MAPE(test_all$Target_Variable, null_pred)
RMSE(test_all$Target_Variable, null_pred)
MAE(test_all$Target_Variable, null_pred)








