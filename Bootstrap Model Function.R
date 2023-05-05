# This script contains the function for the bootstrap model

library(readr)
library(tidyr)
library(dplyr)

# Read in data and select variables of interest
CountyYields <- read_csv("C:/Users/Joseph/Desktop/EFIN 499/County_Yields")
data <- CountyYields |> select(cropYear, commodityCode, yield, acres, CountyName, FIPS)

# Create function for Bootstrap Yields
bootstrap_yield <- function(data, year, commodity, fips, conf_level, total_acres = 0) {
  ## ARGS: 
  # data: data frame with column names of "cropYear', 'commodityCode', 'FIPS', 'yield', and 'acres'
  # year: ex: 2016
  # commodity: code, ex: 41
  # fips: state and county code, ex: 17105
  # conf_level: confidence level, ex: 99
  # total_acres: total acres of all farms insured within county
  
  ## Output: graph of 1000 trajectories for yields and a graph of the interquartile range (IQR)
  
  # Filter data based on input
  filtered_data <- data |> filter(cropYear == year, commodityCode == commodity, FIPS == fips) |> select(yield, acres, CountyName)
  
  # Set the percentage of yields and acres reported
  percent_reported <- seq(0.05, 1, 0.005)
  percent_numeric <- percent_reported * 100
  
  # Set the number of bootstrap samples to use
  n_boot <- 1000
  
  # Create empty matrix to store yhat values
  yhat_matrix <- matrix(NA, nrow = n_boot, ncol = length(percent_reported))
  colnames(yhat_matrix) <- paste0(percent_reported * 100, "%")
  
  # Calculate total acres in county
  if (missing(total_acres)) {
    total_acres <- sum(filtered_data$acres)
  } else {
    total_acres <- total_acres
  }
  
  for (i in 1:n_boot) {
    # Randomly sample to get random order of farms reported
    rows <- sample(nrow(filtered_data), size = nrow(filtered_data), replace = FALSE)
    df <- filtered_data[rows,]
    
    # Calculate county yield per acre of farms reported
    sum_acres <- cumsum(df$acres)
    sum_yield <- cumsum(df$yield)
    production <- df$yield * df$acres
    sum_production <- cumsum(production)
    yhat <- sum_production / sum_acres
    
    # Get percentage of acres reported
    subset_percent <- sum_acres / total_acres
    
    # Find the index of the closest percentage in the subset_percent vector
    apick <- sapply(percent_reported, function(x) which.max(subset_percent >= x))
    
    # Extract yhat values at the specific indices
    yhat_sub <- yhat[apick]
    
    # Add yhat values to matrix
    yhat_matrix[i,] <- yhat_sub
  }
  
  # Graph trajectories for yield per acre by percentage of acres reported
  matplot(percent_numeric, t(yhat_matrix), type = 'l', xlab = 'Percent of Acres Reported',
          ylab = 'Yield per Acre', main = paste0(filtered_data$CountyName[1], " ", year))
  
  # Calculate the upper and lower quantiles for each percentage of acres reported
  upper_quantile <- conf_level/100 + 0.5*(1 - conf_level/100)
  lower_quantile <- 1 - upper_quantile
  q_upper <- apply(yhat_matrix, 2, quantile, upper_quantile)
  q_lower <- apply(yhat_matrix, 2, quantile, lower_quantile)
  lines(percent_numeric, q_upper, col = 'black', lty = 1, lwd = 5)
  lines(percent_numeric, q_lower, col = 'black', lty = 1, lwd = 5)
  
  # Plot Interquartile range
  iqr = q_upper - q_lower
  matplot(percent_reported * 100, iqr, type = 'l', ylab = 'IQR',
          xlab = 'Percent of Acres Reported', main = paste0(filtered_data$CountyName[1], " ", year))
}

# Test Function
bootstrap_yield(data, 2016, 41, 17105, 99, 100000)
bootstrap_yield(data, 2015, 41, 17001, 95)
bootstrap_yield(data, 2014, 81, 17113, 90)
