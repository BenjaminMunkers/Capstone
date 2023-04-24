library(tidyverse)
library(tictoc)
library(matrixStats)

# Read in data
main_df <- read.csv("C:/Users/Joseph/Desktop/EFIN 499/county_merged_drought_data.csv")

# Set the confidence level
conf_level <- 0.99

# Create an empty dataframe to store the results
weighted_results_df <- data.frame(Crop = integer(),
                         Year = integer(),
                         County = integer(),
                         Percent_Reported = integer(),
                         Mean_Yield = numeric(),
                         SD_Yield = numeric(),
                         Lower_CI = numeric(),
                         Upper_CI = numeric())

# Define variables for progress bar
total_iterations <- length(unique(main_df$cropYear)) *
  length(unique(main_df$commodityCode)) * length(unique(main_df$FIPS))

counter <- 0

tic()
# Iterate over each combination of crop type, year, and county
for (crop in unique(main_df$commodityCode)) {
  for (year in unique(main_df$cropYear)) {
    for (county in unique(main_df$FIPS)) {
      counter <- counter + 1
      
      # Subset the data for the current county, year, and crop type
      subset_data <- subset(main_df, commodityCode == crop & cropYear == year & FIPS == county)
      
      # Calculate the total number of acres
      total_acres <- sum(subset_data$acres)
      
      # Iterate over each percentage of yields reported
      for (i in seq(5, 100, by = 1)) {
        # Calculate the number of observations to include in the confidence interval
        n <- round(nrow(subset_data) * i / 100)
        # Perform a random sample of the yields and acres
        sampled_data <- subset_data[sample(nrow(subset_data), n), ]
        # Calculate the weighted mean and standard deviation of the sampled yields
        yield <- sampled_data$yield * sampled_data$acres
        mean_yield <- weighted.mean(yield, sampled_data$acres)
        sd_yield <- sqrt(weightedVar(yield, sampled_data$acres))
        # Calculate the confidence interval based on the t-distribution
        t_value <- qt(conf_level, df = n - 1)
        me <- t_value * (sd_yield / sqrt(n))
        # Append the results as a row to the data frame
        weighted_results_df <- rbind(weighted_results_df, data.frame(Crop = crop,
                                                   Year = year,
                                                   County = county,
                                                   Percent_Reported = i,
                                                   Mean_Yield = mean_yield,
                                                   Lower_CI = mean_yield - me,
                                                   Upper_CI = mean_yield + me,
                                                   stringsAsFactors = FALSE))
      }
      # Print progress
      cat(paste0("Progress: ", counter, "/", total_iterations, "\n"))
    }
  }
}
toc() # 19492.12 sec elapsed
write.csv(weighted_results_df, "C:/Users/Joseph/Desktop/EFIN 499/weighted_mean_variance_ci.csv")

# Graph corn results
library(ggplot2)
weighted_results_df |> filter(Crop == 41 & Year == 2016 & County == 17007) |>
  ggplot(aes(x = Percent_Reported, y = Mean_Yield)) + geom_line(aes(y = Mean_Yield)) +
  geom_smooth(aes(y = Lower_CI, color = 'Lower 99% CI')) +
  geom_smooth(aes(y = Upper_CI, color = 'Upper 99% CI'))


counties <- unique(weighted_results_df$County)[1:9]

test <- main_df |> select(FIPS, CountyName) |> unique()
test$County <- test$FIPS
weighted_results_df <- merge(weighted_results_df, test, by = "County")

weighted_results_df %>%
  filter(Crop == 41 & Year == 2016 & County %in% counties) %>%
  ggplot(aes(x = Percent_Reported, y = Mean_Yield)) + 
  geom_line(aes(y = Mean_Yield)) +
  geom_smooth(aes(y = Lower_CI, color = 'Lower 99% CI')) + 
  geom_smooth(aes(y = Upper_CI, color = 'Upper 99% CI')) +
  facet_wrap(~CountyName, nrow = 3, ncol = 3, scales = 'free') +
  labs(x = "Percentage of yields reported", y = "Weighted mean yield", 
       title = "Weighted Mean Corn Yield and Confidence Intervals by County",
       color = "Confidence Interval") +
  theme(plot.title = element_text(hjust = 0.5))

# Graph soy results
weighted_results_df %>%
  filter(Crop == 81 & Year == 2016 & County %in% counties) %>%
  ggplot(aes(x = Percent_Reported, y = Mean_Yield)) + 
  geom_line(aes(y = Mean_Yield)) +
  geom_smooth(aes(y = Lower_CI, color = 'Lower 99% CI')) + 
  geom_smooth(aes(y = Upper_CI, color = 'Upper 99% CI')) +
  facet_wrap(~CountyName, nrow = 3, ncol = 3, scales = 'free') +
  labs(x = "Percentage of yields reported", y = "Weighted mean yield", 
       title = "Weighted Mean Soy Yield and Confidence Intervals by County",
       color = "Confidence Interval") +
  theme(plot.title = element_text(hjust = 0.5))
