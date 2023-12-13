
source('src/R/Bayes-Model-Functions.R')

library(rstanarm)
library(rstan)

fv5_train1 <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train1.csv')
fv5_train2 <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train2.csv')

fv5_train <- rbind(fv5_train1, fv5_train2)


# Modeling ----------------------------------------------------------------
tmp <- fv5_train[sample(c(1:nrow(fv5_train)), 5000),]

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


NegBinBayes_laplace_mse <- customizedRandomizedSearchCV(data = fv5_train, 
                                                       formula = Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                                                            Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                                                            Post_Length_logNorm + Post_Share_Count_logNorm + 
                                                            CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + (1|Category),
                                                        fold_number = 5, stratified_target = 'Category',
                                                        MCMC_parms = list(chains = 4, iter = 2000, refresh = 1, adapt_delta = 0.95, QR = TRUE, sparse = FALSE),
                                                        randomseed = 2023, lambda_dist = rexp, n = 5, rate = 0.8)
