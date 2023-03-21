
# Run CountyTotalYields first to get the merged data.
# Best to save merged data to be used so it isn't needed to make every time
main_df = read.csv("C:/Users/Joseph/Desktop/EFIN 499/county_merged_drought_data.csv")
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

percent_reported = seq(0.1,0.9,0.05)*100
results_df = as.data.frame(cbind(percent_reported,base_value,standard_errors))

library(ggplot2)
results_df$ci90upper = results_df$base_value + 1.64*results_df$standard_errors
results_df$ci90lower = results_df$base_value - 1.64*results_df$standard_errors

results_df$ci95upper = results_df$base_value + 1.96*results_df$standard_errors
results_df$ci95lower = results_df$base_value - 1.96*results_df$standard_errors

results_df$ci99upper = results_df$base_value + 3.29*results_df$standard_errors
results_df$ci99lower = results_df$base_value - 3.29*results_df$standard_errors


ggplot(results_df, aes(x = percent_reported, y = value)) +
  geom_line(aes(y= base_value, col='base_value')) + 
  geom_line(aes(y=ci90lower, color = 'Lower 90% CI')) + 
  geom_line(aes(y=ci90upper, color = 'Upper 90% CI')) +
  geom_line(aes(y=ci95lower, color = 'Lower 95% CI')) + 
  geom_line(aes(y=ci95upper, color = 'Upper 95% CI')) +
  geom_line(aes(y=ci99lower, color = 'Lower 99% CI')) + 
  geom_line(aes(y=ci99upper, color = 'Upper 99% CI')) +
  labs(x = 'Percent Reported', y = "Yield", title = "Preliminary Model", color = "Legend" ) +
  scale_color_manual(values = c('Lower 90% CI' = 'blue','Upper 90% CI' = 'blue',
                              'Lower 95% CI' = 'orange','Upper 95% CI' = 'orange',
                              'Lower 99% CI' = 'red', 'Upper 99% CI' = 'red'),
                   labels = c('Lower 90% CI','Lower 95% CI','Lower 99% CI','Upper 90% CI','Upper 95% CI','Upper 99% CI'))
  
  

                          