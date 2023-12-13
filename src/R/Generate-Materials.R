library(stargazer)


# Summary Stats of Data ---------------------------------------------------

stargazer(fv5, type = 'text')
stargazer::stargazer(coef(summary(MixEffZeroInfNegBinFit_FS))$cond, type = 'text', title = 'Mixed Effect Zero Inflation Negative Binomial Regression Estimation - Negative Binomial Part')
stargazer::stargazer(coef(summary(MixEffZeroInfNegBinFit_FS))$zi, type = 'text', title = 'Mixed Effect Zero Inflation Negative Binomial Regression Estimation - Zero Inflation Part')
coef(summary(MixEffZeroInfNegBinFit_FS))

# Feature selected ME ZI NB model -----------------------------------------

load('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/FeatureSelected_MixEffZeroInfNegBinFreqFit.RDS')
stargazer::stargazer(coef(summary(MixEffZeroInfNegBinFit_FS))$cond, type = 'text', title = 'Feature Selected Mixed Effect Zero Inflation Negative Binomial Regression Estimation - Negative Binomial Part')
stargazer::stargazer(coef(summary(MixEffZeroInfNegBinFit_FS))$zi, type = 'text', title = 'Feature Selected Mixed Effect Zero Inflation Negative Binomial Regression Estimation - Zero Inflation Part')
summary(MixEffZeroInfNegBinFit_FS)

# $cond
#                                   Estimate   Std. Error     z value      Pr(>|z|)
# (Intercept)                   -0.781461355 1.181368e-01   -6.614885  3.718419e-11
# CC4_logNorm                   -0.709031726 6.610852e-03 -107.252706  0.000000e+00
# CC5                            0.003636943 2.918775e-05  124.605116  0.000000e+00
# Page_Popularity_Likes_logNorm  0.409808159 1.721152e-02   23.810115 2.623994e-125
# Page_Checkins_logNorm         -0.110526434 9.259133e-03  -11.937018  7.589543e-33
# Page_Talking_About_logNorm     0.490097606 1.655990e-02   29.595444 1.710245e-192
# Post_Length_logNorm           -0.044690553 8.719567e-03   -5.125318  2.970360e-07
# Post_Share_Count_logNorm       0.814973392 8.733581e-03   93.314913  0.000000e+00
# 
# $zi
#                                   Estimate   Std. Error     z value      Pr(>|z|)
# (Intercept)                   -1.469080757 0.0232943433 -63.0659871  0.000000e+00
# CC4_logNorm                   -1.449302699 0.0172225617 -84.1514011  0.000000e+00
# CC5                           -0.005419831 0.0001647283 -32.9016492 2.081670e-237
# Page_Popularity_Likes_logNorm  0.073596041 0.0182174054   4.0398750  5.347970e-05
# Page_Checkins_logNorm          0.009096801 0.0123456459   0.7368429  4.612179e-01
# Page_Talking_About_logNorm    -0.316986046 0.0186656730 -16.9822993  1.110489e-64
# Post_Length_logNorm           -0.066034395 0.0106688412  -6.1894627  6.036963e-10
# Post_Share_Count_logNorm      -0.717340340 0.0185732656 -38.6221979  0.000000e+00

# Mixed effect zero inflation negative binomial estimation ----------------
load('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/MixEffZeroInfNegBinFreqFit.RDS')
stargazer::stargazer(coef(summary(MixEffZeroInfNegBinFit))$cond, type = 'text', title = 'Mixed Effect Zero Inflation Negative Binomial Regression Estimation - Negative Binomial Part')
stargazer::stargazer(coef(summary(MixEffZeroInfNegBinFit))$zi, type = 'text', title = 'Mixed Effect Zero Inflation Negative Binomial Regression Estimation - Zero Inflation Part')
coef(summary(MixEffZeroInfNegBinFit))

# $cond
#                                   Estimate   Std. Error    z value      Pr(>|z|)
# (Intercept)                   -2.032477078 9.551587e-02 -21.278946 1.778983e-100
# CC1_logNorm                    2.127289988 1.100295e-01  19.333822  2.789670e-83
# CC2_logNorm                    1.355493630 1.584485e-02  85.547878  0.000000e+00
# CC3_logNorm                   -0.366207658 1.668286e-02 -21.951131 8.447287e-107
# CC4_logNorm                   -6.686403686 1.093800e-01 -61.130051  0.000000e+00
# CC5                            0.002318047 5.455403e-05  42.490858  0.000000e+00
# Page_Popularity_Likes_logNorm  0.163987158 9.880830e-03  16.596496  7.387942e-62
# Page_Checkins_logNorm         -0.065729926 5.078589e-03 -12.942558  2.588647e-38
# Page_Talking_About_logNorm     0.427383899 1.028234e-02  41.564859  0.000000e+00
# Post_Length_logNorm           -0.009572887 4.676671e-03  -2.046945  4.066352e-02
# Post_Share_Count_logNorm       0.531691457 5.104767e-03 104.155868  0.000000e+00
# CC2_per_hr_logNorm            -1.300801686 2.213140e-02 -58.776290  0.000000e+00
# CC3_per_hr_logNorm             0.384027902 1.357008e-02  28.299609 3.494428e-176
# CC4_per_hr_logNorm             3.505744660 2.420654e-02 144.826344  0.000000e+00
# 
# $zi
#                                   Estimate  Std. Error    z value      Pr(>|z|)
# (Intercept)                    -6.22289921 0.129900963 -47.904950  0.000000e+00
# CC1_logNorm                    -0.26413460 0.125712812  -2.101095  3.563260e-02
# CC2_logNorm                    -0.89022702 0.065723052 -13.545126  8.466835e-42
# CC3_logNorm                    -0.19285191 0.080045395  -2.409282  1.598395e-02
# CC4_logNorm                     2.39363118 0.138060564  17.337545  2.449810e-67
# CC5                             0.02571428 0.001596947  16.102157  2.463656e-58
# Page_Popularity_Likes_logNorm  -0.02429657 0.016156249  -1.503850  1.326201e-01
# Page_Checkins_logNorm           0.02965048 0.010686724   2.774515  5.528407e-03
# Page_Talking_About_logNorm     -0.23158044 0.017137302 -13.513238  1.306457e-41
# Post_Length_logNorm            -0.07386900 0.009228724  -8.004249  1.201980e-15
# Post_Share_Count_logNorm       -0.49861676 0.015089165 -33.044689 1.854323e-239
# CC2_per_hr_logNorm              1.86426216 0.363185031   5.133092  2.850206e-07
# CC3_per_hr_logNorm              0.79783589 0.256809718   3.106720  1.891755e-03
# CC4_per_hr_logNorm            -13.86569205 0.377046304 -36.774507 4.719307e-296


# Null Negative Binomial Regression Model ---------------------------------

stargazer::stargazer(coef(summary(zInfNegBinNullModel))$cond, type = 'text')

# Zero inflation negative binomial estimation ----------------------------------------
load('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/zInfNegBinFreqFit_noMixEff.RDS')

stargazer::stargazer(coef(summary(zInfNegBinFit_noMixEff))$count, type = 'text', title = 'Zero Inflation Negative Binomial Estimation - Negative Binomial Part')
stargazer::stargazer(coef(summary(zInfNegBinFit_noMixEff))$zero, type = 'text', title = 'Zero Inflation Negative Binomial Estimation - Zero Inflation Part')
coef(summary(zInfNegBinFit_noMixEff))

# $count
#                                   Estimate   Std. Error     z value      Pr(>|z|)
# (Intercept)                   -1.559174875 6.151583e-03 -253.459137  0.000000e+00
# CC1_logNorm                    2.207655091 1.146547e-01   19.254813  1.286393e-82
# CC2_logNorm                    1.404909268 1.629309e-02   86.227307  0.000000e+00
# CC3_logNorm                   -0.313771997 1.727035e-02  -18.168251  9.208697e-74
# CC4_logNorm                   -6.787745395 1.137605e-01  -59.666976  0.000000e+00
# CC5                            0.002263206 5.472928e-05   41.352750  0.000000e+00
# Page_Popularity_Likes_logNorm  0.024381845 7.822043e-03    3.117069  1.826590e-03
# Page_Checkins_logNorm         -0.141438246 4.192760e-03  -33.733925 1.839839e-249
# Page_Talking_About_logNorm     0.473901340 7.967537e-03   59.479024  0.000000e+00
# Post_Length_logNorm           -0.031224603 4.606303e-03   -6.778669  1.212878e-11
# Post_Share_Count_logNorm       0.509958081 4.870930e-03  104.694198  0.000000e+00
# CC2_per_hr_logNorm            -1.339873040 2.269958e-02  -59.026349  0.000000e+00
# CC3_per_hr_logNorm             0.339781554 1.399175e-02   24.284428 2.863458e-130
# CC4_per_hr_logNorm             3.563052510 2.481787e-02  143.568011  0.000000e+00
# Log(theta)                    -0.333275955 5.386785e-03  -61.869171  0.000000e+00
# 
# $zero
#                                   Estimate  Std. Error     z value      Pr(>|z|)
# (Intercept)                   -7.271041143 0.096755805 -75.1483709  0.000000e+00
# CC1_logNorm                   -2.402552370 0.129488026 -18.5542435  7.536079e-77
# CC2_logNorm                   -0.517236469 0.061730004  -8.3790124  5.337367e-17
# CC3_logNorm                   -0.015276133 0.074566559  -0.2048657  8.376770e-01
# CC4_logNorm                    2.276976783 0.139427712  16.3308768  5.952273e-60
# CC5                            0.015679063 0.001420819  11.0352273  2.583974e-28
# Page_Popularity_Likes_logNorm -0.037415875 0.017120848  -2.1853984  2.885965e-02
# Page_Checkins_logNorm          0.005995728 0.011335727   0.5289232  5.968588e-01
# Page_Talking_About_logNorm    -0.065532126 0.018034079  -3.6337939  2.792840e-04
# Post_Length_logNorm           -0.040993649 0.009780082  -4.1915444  2.770619e-05
# Post_Share_Count_logNorm      -0.259482022 0.015695091 -16.5326869  2.134147e-61
# CC2_per_hr_logNorm             1.651819499 0.329957717   5.0061551  5.552803e-07
# CC3_per_hr_logNorm             0.378653125 0.228226826   1.6591088  9.709388e-02
# CC4_per_hr_logNorm            -8.353611161 0.341140540 -24.4873012 2.017110e-132


# Quasi-Poisson estimation -----------------------------------------------------------
load('~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Model-Object/quasiPoisFreqFit.RDS')
stargazer::stargazer(coef(summary(quasiPoisFit)), type = 'text', title = 'Quasi-Poisson Regression Estimation')
coef(summary(quasiPoisFit))

#                                    Estimate   Std. Error    t value      Pr(>|t|)
# (Intercept)                   -2.9998850759 4.287964e-02 -69.960586  0.000000e+00
# CC1_logNorm                   -2.6908446497 2.946003e-01  -9.133883  6.667943e-20
# CC2_logNorm                    2.4834502625 1.179759e-01  21.050483  2.899015e-98
# CC3_logNorm                    0.3499543251 6.639162e-02   5.271062  1.357777e-07
# CC4_logNorm                   -1.2094206914 2.785713e-01  -4.341512  1.415756e-05
# CC5                           -0.0008141404 7.044393e-05 -11.557283  6.938078e-31
# Page_Popularity_Likes_logNorm -0.0821466594 3.220571e-02  -2.550686  1.075184e-02
# Page_Checkins_logNorm         -0.1705922653 1.425649e-02 -11.965933  5.500882e-33
# Page_Talking_About_logNorm     0.8066551963 4.050344e-02  19.915721  3.625776e-88
# Post_Length_logNorm            0.0252864905 1.674578e-02   1.510022  1.310395e-01
# Post_Share_Count_logNorm       0.4506685374 1.846088e-02  24.412084 1.989596e-131
# CC2_per_hr_logNorm            -0.8208150049 1.077213e-01  -7.619805  2.551733e-14
# CC3_per_hr_logNorm            -0.3316261424 4.414740e-02  -7.511793  5.856503e-14
# CC4_per_hr_logNorm             2.3336583871 1.159748e-01  20.122124  5.808576e-90


