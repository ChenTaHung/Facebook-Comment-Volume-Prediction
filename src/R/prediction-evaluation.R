source('src/R/validation-functions.R')

test_all <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/testall.csv')

load('data/Model-Object/zInfNegBinBayesFit.RDS')
load('data/Model-Object/zInfNegBinFreqFit.RDS')


# Fitted Model ------------------------------------------------------------

MSE(fv5_train$Target_Variable, fitted(zInfNegBinFit))
MAPE(fv5_train$Target_Variable, fitted(zInfNegBinFit))
RMSE(fv5_train$Target_Variable, fitted(zInfNegBinFit))
MAE(fv5_train$Target_Variable, fitted(zInfNegBinFit))


# Prediction --------------------------------------------------------------
testFreqPred <- predict(zInfNegBinFit, newdata = test_all, type = 'terms')

MSE(test_all$Target_Variable, testFreqPred)
MAPE(test_all$Target_Variable, testFreqPred)
RMSE(test_all$Target_Variable, testFreqPred)
MAE(test_all$Target_Variable, testFreqPred)
