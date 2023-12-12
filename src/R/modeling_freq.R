library(tidyverse)
library(ggplot2)
library(rstanarm)
library(lme4)
library(optimx)
require(sjPlot)
library(pscl)
library(glmmTMB)

# import data -------------------------------------------------------------

fv5_train1 <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train1.csv')
fv5_train2 <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train2.csv')
fv5_train <- rbind(fv5_train1, fv5_train2)



# Modeling ----------------------------------------------------------------


# Mixed Effect ------------------------------------------------------------

zInfNegBinFit <- glmmTMB(formula = Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + 
                            offset(log(Base_Time+1)) + (1|Category), 
                          ziformula = ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm,
                          data = fv5_train, family = nbinom2)

summary(zInfNegBinFit)

# save(zInfNegBinFit, file = '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/zInfNegBinFreqFit.RDS')

zInfNegBinNullModel <- glmmTMB(formula = Target_Variable ~ 1, data = fv5_train, family = nbinom2)

summary(zInfNegBinNullModel)



# No mixed effect ---------------------------------------------------------


zInfNegBinFit_noRandEff <- zeroinfl(formula = Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm +
                            offset(log(Base_Time+1)) , data = fv5_train, dist = 'negbin')

summary(zInfNegBinFit_noRandEff)

# save(zInfNegBinFit_noRandEff, file = '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/zInfNegBinFreqFit_noRandEff.RDS')

zInfNegBinFitNullModel_noRandEff <- zeroinfl(formula = Target_Variable ~ 1, data = fv5_train, dist = 'negbin')









































# Poisson -----------------------------------------------------------------

tmp <- fv5_train[sample(c(1:nrow(fv5_train)), 5000),]

quasiPoisFit <- glm(formula = Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + 
                            offset(log(Base_Time+1)), data = tmp, family = quasipoisson())


plot_model(freqFit, type = 'est')


summary(quasiPoisFit)
confint(quasiPoisFit)
