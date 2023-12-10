
library(rstanarm)
library(rstan)

fv5_train1 <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train1.csv')
fv5_train2 <- read_csv('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train2.csv')
fv5_train <- rbind(fv5_train1, fv5_train2)


# Modeling ----------------------------------------------------------------
tmp <- fv5_train[sample(c(1:nrow(fv5_train)), 5000),]

options(mc.cores = 4)

zInfNegBinBayesFit <- rstanarm::stan_glmer(Target_Variable ~ CC1_logNorm + CC2_logNorm + CC3_logNorm + CC4_logNorm + CC5 + 
                                  Page_Popularity_Likes_logNorm + Page_Checkins_logNorm +  Page_Talking_About_logNorm + 
                                  Post_Length_logNorm + Post_Share_Count_logNorm + 
                                  CC2_per_hr_logNorm + CC3_per_hr_logNorm + CC4_per_hr_logNorm + (1|Category),
                                 data = fv5_train, family = neg_binomial_2(link = "log"))

summary(zInfNegBinBayesFit)
apply(coef(zInfNegBinBayesFit)$Category, 2, mean)
se(zInfNegBinBayesFit)

plot_model(zInfNegBinBayesFit,type = 'diag')

# save(zInfNegBinBayesFit, file = '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/zInfNegBinBayesFit.RDS')

