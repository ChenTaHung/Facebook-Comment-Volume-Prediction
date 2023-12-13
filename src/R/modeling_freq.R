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


# Feature Selected Mixed Effect ZI NB --------------------------------------------------------

MixEffZeroInfNegBinFit_FS <- glmmTMB(formula = Target_Variable ~ CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            offset(log(Base_Time+1)) + (1|Category), 
                          ziformula = ~ CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm,
                          data = fv5_train, family = nbinom2)

summary(MixEffZeroInfNegBinFit_FS)

save(MixEffZeroInfNegBinFit_FS, file = '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/FeatureSelected_MixEffZeroInfNegBinFreqFit.RDS')

# Mixed Effect ------------------------------------------------------------

MixEffZeroInfNegBinFit <- glmmTMB(formula = Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + 
                            offset(log(Base_Time+1)) + (1|Category), 
                          ziformula = ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm,
                          data = fv5_train, family = nbinom2)

summary(MixEffZeroInfNegBinFit)

save(MixEffZeroInfNegBinFit, file = '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/MixEffZeroInfNegBinFreqFit.RDS')


# NB Null -----------------------------------------------------------------

zInfNegBinNullModel <- glmmTMB(formula = Target_Variable ~ 1, data = fv5_train, family = nbinom2)

summary(zInfNegBinNullModel)



# No mixed effect Zero Inflation ---------------------------------------------------------


zInfNegBinFit_noMixEff <- zeroinfl(formula = Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm +
                            offset(log(Base_Time+1)) , data = fv5_train, dist = 'negbin')

summary(zInfNegBinFit_noMixEff)

save(zInfNegBinFit_noRandEff, file = '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/zInfNegBinFreqFit_noMixEff.RDS')

zInfNegBinFitNullModel_noRandEff <- zeroinfl(formula = Target_Variable ~ 1, data = fv5_train, dist = 'negbin')



# Poisson -----------------------------------------------------------------

tmp <- fv5_train[sample(c(1:nrow(fv5_train)), 5000),]

quasiPoisFit <- glm(formula = Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + 
                            offset(log(Base_Time+1)), data = fv5_train, family = quasipoisson())


save(quasiPoisFit,file = '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/quasiPoisFreqFit.RDS')


summary(quasiPoisFit)
confint(quasiPoisFit)
