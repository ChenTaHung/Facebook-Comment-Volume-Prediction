library(tidyverse)
library(data.table)
setwd('Final-Project/Remote-Git/')
coln <- fread('Final-Project/Remote-Git/data/Dataset/FeatureNames.csv')
coln[, feature := gsub(' ', "_", gsub('[[:punct:]]', '', feature))]

Category <- fread('Final-Project/Remote-Git/data/Dataset/Converted_Category_File.csv', sep = ',', sep2 = NULL)
Category[, index := .I]

fv1 <- fread('Final-Project/Remote-Git/data/Dataset/Training/Features_Variant_1.csv', col.names = coln$feature)
fv2 <- fread('Final-Project/Remote-Git/data/Dataset/Training/Features_Variant_2.csv', col.names = coln$feature)
fv3 <- fread('Final-Project/Remote-Git/data/Dataset/Training/Features_Variant_3.csv', col.names = coln$feature)
fv4 <- fread('Final-Project/Remote-Git/data/Dataset/Training/Features_Variant_4.csv', col.names = coln$feature)
fv5 <- fread('Final-Project/Remote-Git/data/Dataset/Training/Features_Variant_5.csv', col.names = coln$feature)


test1 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_1.csv', col.names = coln$feature)
test2 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_2.csv', col.names = coln$feature)
test3 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_3.csv', col.names = coln$feature)
test4 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_4.csv', col.names = coln$feature)
test5 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_5.csv', col.names = coln$feature)
test6 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_6.csv', col.names = coln$feature)
test7 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_7.csv', col.names = coln$feature)
test8 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_8.csv', col.names = coln$feature)
test9 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_9.csv', col.names = coln$feature)
test10 <- fread('Final-Project/Remote-Git/data/Dataset/Testing/TestSet/Test_Case_10.csv', col.names = coln$feature)


# Mapping the Category Data -----------------------------------------------

datasets <- list(fv1=fv1, fv2=fv2, fv3=fv3, fv4=fv4, fv5=fv5, 
                 test1=test1, test2=test2, test3=test3, test4=test4, test5=test5, test6=test6, test7=test7, test8=test8, test9=test9, test10=test10)


# Merging Category data
datasets <- lapply(datasets, function(dataset) {
    merge(dataset, Category, by.x="Page_Category", by.y="index", all.x=TRUE)
})

# Assigning merged datasets back to individual variables
list2env(datasets, envir = .GlobalEnv)
