
source('importdata.R')
library(ggalt)

# Create Volumn per hour --------------------------------------------------

# Since C1 = C2 + C3 + C4 across the base_time duration

fv5[, CC2_per_hr := ifelse(is.na(CC2/ Base_Time), 0, CC2/ Base_Time)]
fv5[, CC3_per_hr := ifelse(is.na(CC3/ Base_Time), 0, CC3/ Base_Time)]
fv5[, CC4_per_hr := ifelse(is.na(CC4/ Base_Time), 0, CC4/ Base_Time)]

ggplot(fv5, aes(CC1, Page_Popularity_Likes, colour=Category)) +
    geom_point() +
    geom_encircle(aes(group=Category)) +
    theme(legend.position="none")

# we can see that the scale is large and vary a lot across different columns

# scaling the features : Log transformation and then normalization

cols_to_log_normalize <- c('CC1', 'CC2', 'CC3', 'CC4', 'Page_Popularity_Likes', 'Page_Checkins', 'Page_Talking_About', 'Post_Length', 'Post_Share_Count', 
                   'CC2_per_hr', 'CC3_per_hr', 'CC4_per_hr')

new_log_norm_names <- paste0(cols_to_log_normalize, "_logNorm")

for (i in seq_along(cols_to_log_normalize)) {
  fv5[, (new_log_norm_names[i]) := scale(log1p(.SD)), .SDcols = c(cols_to_log_normalize[i])]
}

# Visualization after 

ggplot(fv5, aes(CC1_logNorm, Page_Popularity_Likes_logNorm, colour=Category)) +
    geom_point() +
    geom_encircle(aes(group=Category)) +
    theme(legend.position="none") 

# And now it looks better

write_csv(fv5, '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train.csv')


# Test Data ---------------------------------------------------------------

Test_all <- do.call(rbind, list(test1, test2, test3, test4, test5, test6, test7, test8, test9,test10))

Test_all[, CC2_per_hr := ifelse(is.na(CC2/ Base_Time), 0, CC2/ Base_Time)]
Test_all[, CC3_per_hr := ifelse(is.na(CC3/ Base_Time), 0, CC3/ Base_Time)]
Test_all[, CC4_per_hr := ifelse(is.na(CC4/ Base_Time), 0, CC4/ Base_Time)]

# Apply same mean and same std of training datasets.

for (i in seq_along(cols_to_log_normalize)) {
  # mean and std
  colMean <- mean(fv5[, get(cols_to_log_normalize[i])],na.rm = T)
  std <- stats::sd(fv5[, get(cols_to_log_normalize[i])],na.rm = T)
  
  # Log
  logged <- log1p(Test_all[, get(cols_to_log_normalize[i])])
  
  # normalization
  normalized <- (logged-colMean)/std
  
  Test_all[, (new_log_norm_names[i]) := normalized]
  rm(list = c('colMean', 'std', 'logged', 'normalized'))
}

write_csv(Test_all, '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/testall.csv')
