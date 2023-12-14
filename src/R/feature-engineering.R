
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
  theme_bw() +
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
  theme_bw() +
  theme(legend.position="none") 

# And now it looks better

write_csv(fv5[1:100000, ], 'Final-Project/Remote-Git/data/Dataset/fv5_train1.csv')
write_csv(fv5[100001:nrow(fv5), ], 'Final-Project/Remote-Git/data/Dataset/fv5_train2.csv')

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

write_csv(Test_all, 'Final-Project/Remote-Git/data/Dataset/testall.csv')

# Cor Matrix --------------------------------------------------------------

cor_matrix <- cor(fv5[, c("CC1_logNorm", "CC2_logNorm", "CC3_logNorm", "CC4_logNorm", "CC5",
                          "Page_Popularity_Likes_logNorm", "Page_Checkins_logNorm", 
                          "Page_Talking_About_logNorm", "Post_Length_logNorm", 
                          "Post_Share_Count_logNorm", "CC2_per_hr_logNorm", 
                          "CC3_per_hr_logNorm", "CC4_per_hr_logNorm")])

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "", title = "Correlation Matrix Heatmap") 

