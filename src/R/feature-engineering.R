source('importdata.R')


# Create Volumn per hour --------------------------------------------------

# Since C1 = C2 + C3 + C4 across the base_time duration

fv5[, CC2_per_hr := CC2/ Base_Time]
fv5[, CC3_per_hr := CC3/ Base_Time]
fv5[, CC4_per_hr := CC4/ Base_Time]

write.csv(fv5, '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/fv5_train.csv')




# Test Data ---------------------------------------------------------------

Test_all <- do.call(rbind, list(test1, test2, test3, test4, test5, test6, test7, test8, test9,test10))

Test_all[, CC2_per_hr := CC2/ Base_Time]
Test_all[, CC3_per_hr := CC3/ Base_Time]
Test_all[, CC4_per_hr := CC4/ Base_Time]

write.csv(Test_all, '~/Desktop/MSSP/MA678-AppliedStatisticalModeling/Final-Project/Remote-Git/data/Dataset/testall.csv')
