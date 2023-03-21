
# Run CountyTotalYields first to get the merged data.
# Best to save merged data to be used so it isn't needed to make every time
main_df = read.csv("C:/Users/jammu/Desktop/EFIN 499/merged_data.csv")
library(tidyverse)
# Splitting data
split_df = subset(main_df, CountyCode == 1 & commodityCode == 81 & cropYear == 2010)

# finds the average and standard deviation of the data set without weighting by acre
# done in total yields for the county
results = matrix(NA, nrow = 1000, ncol = 1)
train_value = results
true_value = mean(split_df$yield*split_df$acres)
standard_errors = matrix(NA, nrow = 16)
base_value = standard_errors
count = 0
for(j in seq(0.1,0.9,0.05)){
  for(i in 1:1000){
    set.seed(i)
    train = split_df %>% sample_frac(j)
    train_value[i] = mean(train$yield * train$acres)
    results[i] = train_value[i] - true_value
  }
  count = count + 1
  base_value[count] = mean(train_value)
  standard_errors[count]= sd(results)
}

