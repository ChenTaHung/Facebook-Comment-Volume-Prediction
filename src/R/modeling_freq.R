library(tidyverse)
library(ggplot2)
library(rstanarm)
library(lme4)
library(optimx)
require(sjPlot)
library(pscl)
library(glmmTMB)

# import data -------------------------------------------------------------

fv5_train <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train.csv')
test_all <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/testall.csv')


# Modeling ----------------------------------------------------------------



zInfNegBinFit <- glmmTMB(formula = Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + (1|Category), 
                          ziformula = ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm,
                          data = fv5_train, family = nbinom2)

summary(zInfNegBinFit)

# save(zInfNegBinFit, file = '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/zInfNegBinFreqFit.RDS')

zInfNegBinNullModel <- glmmTMB(formula = Target_Variable ~ 1, data = fv5_train, family = nbinom2)

summary(zInfNegBinNullModel)

















































# Poisson -----------------------------------------------------------------

tmp <- fv5_train[sample(c(1:nrow(fv5_train)), 5000),]

freqFit <- glmer.nb(Target_Variable ~ 
                  CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                  Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + Post_Length_logNorm + Post_Share_Count_logNorm + 
                  CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + (1|Category),
                data = tmp)

# https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer


# http://www.strengejacke.de/sjPlot/reference/plot_model.html
plot_model(freqFit, type = 'est')


summary(freqFit)
confint(freqFit)
