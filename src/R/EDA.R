library(GGally)
source('importdata.R')

# CC1 & Target ------------------------------------------------------------

ggplot(fv5) +
  geom_point(aes(CC1, Target_Variable, color = Base_Time), alpha = .5)


ggplot(fv5[CC1 < 750 & Target_Variable < 750 & CC1 > 0 & Target_Variable > 0,]) +
  geom_point(aes(CC1, Target_Variable, color = Base_Time), alpha = .5)

# CC5 and Target ----------------------------------------------------------

ggplot(fv5) +
  geom_point(aes(x = CC5, y = Target_Variable))

# Base_Time & CC1 ---------------------------------------------------------

ggplot(fv5) +
  geom_point(aes(x = Base_Time, y = CC1))

# Base_Time & Target ------------------------------------------------------

ggplot(fv5) +
  geom_point(aes(x = Base_Time, y = Target_Variable))

# Base Time x Target / mean CC1 -------------------------------------------

ggplot() +
  geom_point(data = fv5[, .(mean_target = mean(Target_Variable)), by = Base_Time], 
             aes(x = Base_Time, y = mean_target, color = 'Mean Target')) +
  geom_point(data = fv5[, .(mean_CC1 = mean(CC1)), by = Base_Time], 
             aes(x = Base_Time, y = mean_CC1, color = 'Mean CC1')) +
  ylab('CC1 / Target Variable') +
  theme_bw() +
  scale_color_manual(values = c('Mean Target' = 'blue', 'Mean CC1' = 'orange'),
                     name = "Variables", 
                     labels = c("Mean Target", "Mean CC1")) +
  scale_x_continuous(breaks = seq(0,72, 5))

# Base_Time more -> Mean CC1 less
# Base_Time more -> Mean Target More

# Average volume per Hour -------------------------------------------------

ggplot(data = fv5[, .(avg_vol_hr = mean(CC1 / Base_Time)), Base_Time]) +
  geom_point(aes(x = Base_Time, y = avg_vol_hr, color = 'Average CC1 per Hr')) +
  geom_point(data = fv5[, .(mean_target = mean(Target_Variable)), by = Base_Time], 
             aes(x = Base_Time, y = mean_target, color = 'Mean Target')) +
  scale_color_manual(values = c('Average CC1 per Hr' = 'blue', 'Mean Target' = 'orange'),
                     name = "Variables", 
                     labels = c("Average CC1 per Hr", "Mean Target"))

# Average CC1 line is lower than Mean Target

ggplot(data = fv5[, .(avg_vol_hr = mean(CC2 / Base_Time)), Base_Time]) +
  geom_point(aes(x = Base_Time, y = avg_vol_hr, color = 'Average CC1 per Hr')) +
  geom_point(data = fv5[, .(mean_target = mean(Target_Variable)), by = Base_Time], 
             aes(x = Base_Time, y = mean_target, color = 'Mean Target')) +
  scale_color_manual(values = c('Average CC2 per Hr' = 'blue', 'Mean Target' = 'orange'),
                     name = "Variables", 
                     labels = c("Average CC2 per Hr", "Mean Target"))


ggplot(data = fv5[, .(avg_vol_hr = mean(CC4 / Base_Time)), Base_Time]) +
  geom_point(aes(x = Base_Time, y = avg_vol_hr, color = 'Average CC1 per Hr')) +
  geom_point(data = fv5[, .(mean_target = mean(Target_Variable)), by = Base_Time], 
             aes(x = Base_Time, y = mean_target, color = 'Mean Target')) +
  scale_color_manual(values = c('Average CC4 per Hr' = 'blue', 'Mean Target' = 'orange'),
                     name = "Variables", 
                     labels = c("Average CC4 per Hr", "Mean Target"))

# C4 to Target ------------------------------------------------------------

ggplot(fv5[, .(CC4_log_prop = log(((CC4+1) / (Target_Variable+1))+0.1))]) +
  geom_histogram(aes(CC4_log_prop))



# CC2 - CC4 proportion ----------------------------------------------------

tmp <- melt(fv5[,.(CC2 = sum(CC2), CC3 = sum(CC3), CC4 = sum(CC4)), Base_Time], 
            id.vars = 'Base_Time', measure.vars = c('CC2', 'CC3', 'CC4'))[
              , Total := sum(value), Base_Time
              ][, Prop := value / Total]
ggplot(data = tmp) +
  geom_bar(aes(x = Base_Time, y = Prop, fill = variable), position = 'stack', stat = 'identity') +
  scale_x_continuous(breaks = seq(1,72)) +
  theme_bw()

# In Base_Time <= 24, CC2 = CC4 since 
# C2: Comment count in last 24 hrs with respect to selected base date/time.
# C4: Comment count in first 24 hrs after publishing the document, but before the selected base date/time.


# post_length Histogram --------------------------------------------------

par(mfrow = c(2,1))
hist(fv5$Post_Length, main = 'Post Length')
hist(log(fv5$Post_Length), main = 'Log Post Length')
par(mfrow = c(1,1))

# Post share count Histogram ---------------------------------------------

par(mfrow = c(2,1))
hist(fv5$Post_Share_Count)
hist(log(fv5$Post_Share_Count))
par(mfrow = c(1,1))


# Category ----------------------------------------------------------------

ggplot(fv5[,.N,Category]) +
  geom_bar(aes(x = Category, y = N), stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(fv5[, .(mean_target = mean(Target_Variable)), Category]) +
  geom_point(aes(x = Category, y = mean_target, color = mean_target), size = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Post_Published ----------------------------------------------------------

post_publish <- grep("^Post_Published", names(fv5), value = TRUE)
Post_Publish_dt <- fv5[, .SD, .SDcols = c("Target_Variable", "CC1", post_publish)]

ggplot(data = melt(Post_Publish_dt, measure.vars = post_publish)[, .N, .(variable, value)]) +
  geom_bar(aes(x = variable, y = N, fill = factor(value)), stat = 'identity', position = 'dodge') +
  geom_text(aes(x = variable, y = N, label = N, group = factor(value)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, 
            color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.45)) 
  
post_pub_cor_mat <- cor(Post_Publish_dt[, .SD, .SDcols = post_publish])

ggplot(data = reshape2::melt(post_pub_cor_mat), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 45, vjust = 1, hjust = 1))



# Post Share Count --------------------------------------------------------

ggplot(fv5) +
  geom_point(aes(x = log(Post_Share_Count), y = log(Target_Variable)))

# Post Length --------------------------------------------------------

ggplot(fv5) +
  geom_point(aes(x = log(Post_Length), y = log(Target_Variable)))


# CC1 to CC5 --------------------------------------------------------------

ggpairs(fv5[, .SD, .SDcols = paste0('CC', 1:5)], diag = list(continuous = "densityDiag"))


# Page Checkins  ----------------------------------------------------------

ggplot(data = fv5) +
  geom_point(aes(x = Page_Checkins, y = Target_Variable))


# Page Popularity Like ----------------------------------------------------

ggplot(data = fv5[Page_Popularity_Likes <= 20000000]) +
  geom_point(aes(x = Page_Popularity_Likes, y = Target_Variable))

