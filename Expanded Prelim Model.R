# Run CountyTotalYields first to get the merged data.
# Best to save merged data to be used so it isn't needed to make every time

library(tidyverse)
library(tictoc)

# Read in data
main_df = read.csv("C:/Users/Joseph/Desktop/EFIN 499/county_merged_drought_data.csv")


# Set the confidence level
conf_level <- 0.99

# Create an empty dataframe to store the results
results_df <- data.frame(Crop = integer(),
                         Year = integer(),
                         County = integer(),
                         Percent_Reported = integer(),
                         Mean_Yield = numeric(),
                         Lower_CI = numeric(),
                         Upper_CI = numeric())

# Define variables for progress bar
total_iterations <- length(unique(main_df$cropYear)) * length(unique(main_df$commodityCode)) * length(unique(main_df$FIPS))
counter <- 0

tic()
# Iterate over each combination of crop type, year, and county
for (crop in unique(main_df$commodityCode)) {
  for (year in unique(main_df$cropYear)) {
    for (county in unique(main_df$FIPS)) {
      counter <- counter + 1
      
      # Subset the data for the current county, year, and crop type
      subset_data <- subset(main_df, commodityCode == crop & cropYear == year & FIPS == county)
      
      # Iterate over each percentage of yields reported
      for (i in seq(5, 100, by = 5)) {
        # Calculate the number of observations to include in the confidence interval
        n <- round(nrow(subset_data) * i / 100)
        # Perform a random sample of the yields
        sampled_yields <- sample(subset_data$yield, n)
        # Calculate the mean and confidence interval for the sampled yields
        mean_yield <- mean(sampled_yields)
        se <- sd(sampled_yields) # Calculate the standard error of the mean
        # Calculate the confidence interval based on the t-distribution
        t_value <- qt(conf_level, df = n - 1)
        me <- t_value * (se / sqrt(n)) # calculates margin of error
        # Append the results as a row to the dataframe
        results_df <- rbind(results_df, data.frame(Crop = crop,
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
toc()
# 1546.97 sec elapsed

# Print the first few rows of the results dataframe
head(results_df)
