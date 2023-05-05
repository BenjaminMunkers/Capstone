# This script contains the function for the bootstrap model

library(tidyverse)
library(matrixStats)
library(ggplot2)
library(dplyr)

# Read in data and select variables of interest
CountyYields <- read_csv("C:/Users/Joseph/Desktop/EFIN 499/County_Yields")
data <- CountyYields |> select(cropYear, commodityCode, yield, acres, CountyName, FIPS)


# Create function for weighed model
weighted_model <- function(data, year, commodity, fips, conf_level = 99, total_acres = 0) {
  ## ARGS: 
  # data: data frame with column names of "cropYear', 'commodityCode', 'FIPS', 'yield', and 'acres'
  # year: ex: 2016
  # commodity: code, ex: 41
  # fips: state and county code, ex: 17105
  # conf_level: confidence level, ex: 99
  # total_acres: total acres of all farms insured within county
  
  ## Output: graph of yield per acre with weighed confidence interval and plot of the interquartile range (IQR)
  
  # Create an empty dataframe to store the results
  weighted_results_df <- data.frame(Percent_Reported = integer(),
                                    Mean_Yield = numeric(),
                                    SD_Yield = numeric(),
                                    Lower_CI = numeric(),
                                    Upper_CI = numeric())
  # Subset the data for the current county, year, and crop type
  subset_data <- data |> filter(cropYear == year, commodityCode == commodity, FIPS == fips) |> select(yield, acres, CountyName)

  # Calculate total acres in county
  if (missing(total_acres)) {
    total_acres <- sum(subset_data$acres)
  } else {
    total_acres <- total_acres
  }
  
  # Iterate over each percentage of yields reported
  for (i in seq(5, 100, by = 1)) {
    
    # Calculate sum of acres reported so far
    sum_acres <- cumsum(subset_data$acres)
    
    # Get percentage of acres reported
    subset_percent <- sum_acres / total_acres
    
    # Find the index of the closest percentage in the subset_percent vector
    apick <- which.max(subset_percent >= i/100)
    
    # Subset the first x% of the data
    sampled_data <- subset_data[1:apick, ]
    
    # Calculate the weighted mean and standard deviation of the sampled yields
    mean_yield <- weighted.mean(sampled_data$yield, sampled_data$acres)
    sd_yield <- sqrt(weightedVar(sampled_data$yield, sampled_data$acres))
    
    # Calculate the confidence interval based on the t-distribution
    t_value <- qt(conf_level/100, df = apick - 1) # t-value
    se <- t_value * (sd_yield / sqrt(apick)) # standard error
    
    # Append the results as a row to the data frame
    weighted_results_df <- rbind(weighted_results_df, data.frame(Percent_Reported = i,
                                                                 Mean_Yield = mean_yield,
                                                                 SD_Yield = se,
                                                                 Lower_CI = mean_yield - se,
                                                                 Upper_CI = mean_yield + se,
                                                                 stringsAsFactors = FALSE))
  }
  # Graph IQR
  lower_ci <- weighted_results_df$Lower_CI
  upper_ci <- weighted_results_df$Upper_CI
  iqr <- upper_ci - lower_ci
  matplot(weighted_results_df$Percent_Reported, iqr, type = "l", lty = 1, 
          col = c("black"), xlab = "Percentage of yields reported",
          ylab = "IQR", main = paste0(sampled_data$CountyName[1], " ", year))
  # Add legend
  legend("topright", legend = c("IQR"), 
         lty = 1, col = "black", bty = "n")
  
  # Graph Weighted Results
  weighted = ggplot(weighted_results_df, aes(x = Percent_Reported, y = Mean_Yield)) + 
    geom_line(aes(y = Mean_Yield)) +
    geom_smooth(aes(y = Lower_CI, color = paste0("Lower ", conf_level, "% CI"))) + 
    stat_smooth(aes(y = Upper_CI, color = paste0("Upper ", conf_level, "% CI")), se = FALSE) +
    labs(x = "Percentage of yields reported", y = "Weighted Yield", 
         title = paste0(sampled_data$CountyName[1], " ", year),
         color = "Confidence Interval") +
    theme(plot.title = element_text(hjust = 0.5))
  # Call the graph
  weighted
}

# Test Function
weighted_model(data, 2016, 41, 17105, 90, 479281.1)
weighted_model(data, 2015, 41, 17105)
weighted_model(data, 1997, 81, 17113, 95)

