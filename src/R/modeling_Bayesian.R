
source('src/R/Bayes-Model-Functions.R')

library(rstanarm)
library(rstan)
library(tidyverse)

fv5_train1 <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train1.csv')
fv5_train2 <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train2.csv')

fv5_train <- rbind(fv5_train1, fv5_train2)


# Modeling ----------------------------------------------------------------

options(mc.cores = 4)


# NB reg ------------------------------------------------------------------

NegBinBayesFit <- rstanarm::stan_glmer(Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                                  Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                                  Post_Length_logNorm + Post_Share_Count_logNorm + 
                                  CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + (1|Category),
                                 data = fv5_train, family = neg_binomial_2(link = "log"))

summary(NegBinBayesFit)
apply(coef(NegBinBayesFit)$Category, 2, mean)
se(NegBinBayesFit)[1:length(fixef(NegBinBayesFit))]

plot_model(NegBinBayesFit,type = 'est')

pp_check(NegBinBayesFit)

# save(NegBinBayesFit, file = '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/MixEffNegBinBayesFit.RDS')


# NB laplace prior --------------------------------------------------------


NegBinBayes_laplace <- rstanarm::stan_glmer(Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                                               Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                                               Post_Length_logNorm + Post_Share_Count_logNorm + 
                                               CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + (1|Category),
                                             data = fv5_train, family = neg_binomial_2(link = "log"),
                                             prior = laplace(scale = 1, autoscale = TRUE))

save(NegBinBayes_laplace, file = 'data/Model-Object/Bayes/NegBinBayes_laplace.RDS')


# small samples -----------------------------------------------------------
set.seed(2023)
small_fv5 <- fv5_train[sample(c(1:nrow(fv5_train)), 10000),]


NegBinBayesFit_small <- rstanarm::stan_glmer(Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                                                  Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                                                  Post_Length_logNorm + Post_Share_Count_logNorm + 
                                                  CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + (1|Category),
                                                 data = small_fv5, family = neg_binomial_2(link = "log"))


save(NegBinBayesFit_small, file = 'data/Model-Object/Bayes/NegBinBayes_laplace_small.RDS')

NegBinBayes_laplace_small <- rstanarm::stan_glmer(Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                                                     Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                                                     Post_Length_logNorm + Post_Share_Count_logNorm + 
                                                     CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + (1|Category),
                                                   data = small_fv5, family = neg_binomial_2(link = "log"),
                                                   prior = laplace(scale = 1, autoscale = TRUE))

save(NegBinBayes_laplace_small, file = 'data/Model-Object/Bayes/NegBinBayes_laplace_small.RDS')

