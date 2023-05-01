library(readr)
library(tidyr)
library(dplyr)
library(tictoc)
library(ggplot2)

# Read in data and select variables of interest
CountyYields <- read_csv("C:/Users/Joseph/Desktop/EFIN 499/County_Yields")
data <- CountyYields |> select(cropYear, commodityCode, yield, acres, CountyName, FIPS)

################################################################################
# For 2016 (most recent year), select largest county for corn
filtered_df <- data |> filter(cropYear == 2016, commodityCode == 41)

# Group by CountyName and calculate total acres and FIPS code
county_acres <- filtered_df |> group_by(CountyName, FIPS) |> summarise(total_acres = sum(acres))

# Arrange in descending order by total acres and select top 5 counties
top_counties <- county_acres |> arrange(desc(total_acres))

# Print top counties with FIPS codes
print(top_counties[, c("CountyName", "FIPS", "total_acres")])

# Largest county for corn is Livingston County, FIPS = 17105 

################################################################################
# Find largest Soy County

# For 2016 (most recent year), select largest county for soy
filtered_df <- data |> filter(cropYear == 2016, commodityCode == 81)

# Group by CountyName and calculate total acres and FIPS code
county_acres <- filtered_df |> group_by(CountyName, FIPS) %>% summarise(total_acres = sum(acres))

# Arrange in descending order by total acres and select top 5 counties
top_counties <- county_acres |> arrange(desc(total_acres))

# Print top counties with FIPS codes
print(top_counties[, c("CountyName", "FIPS", "total_acres")])

# Largest county for soy is Mclean County, FIPS = 17113 

################################################################################

# Use Livingston County for bootsrapping
Livingston_corn <- data |> filter(cropYear == 2016, commodityCode == 41, FIPS == 17105) |> select(yield, acres)

# Set the percentage of yields and acres reported
percent_reported <- seq(0.05, 1, 0.005)
percent_numeric <- percent_reported * 100

# Set the number of bootstrap samples to use
n_boot <- 1000

# Create empty matrix to store yhat values
yhat_matrix <- matrix(NA, nrow = n_boot, ncol = length(percent_reported))
colnames(yhat_matrix) <- paste0(percent_reported * 100, "%")

tic()
for (i in 1:n_boot) {
  # Randomly sample to get random order of farms reported
  rows <- sample(nrow(Livingston_corn), size = nrow(Livingston_corn), replace = FALSE)
  df <- Livingston_corn[rows,]
  
  # Calculate county yield per acre of farms reported
  sum_acres <- cumsum(df$acres)
  sum_yield <- cumsum(df$yield)
  production <- df$yield * df$acres
  sum_production <- cumsum(production)
  yhat <- sum_production / sum_acres
  
  # Get percentage of acres reported
  total_acres <- sum(df$acres)
  subset_percent <- sum_acres / total_acres
  
  # Find the index of the closest percentage in the subset_percent vector
  apick <- sapply(percent_reported, function(x) which.max(subset_percent >= x))
  
  # Extract yhat values at the specific indices
  yhat_sub <- yhat[apick]
  
  # Add yhat values to matrix
  yhat_matrix[i,] <- yhat_sub
}
toc()

# Graph trajectories for yield per acre by percentage of acres reported
matplot(percent_numeric, t(yhat_matrix), type = 'l', xlab = 'Percent of Acres Reported',
        ylab = 'Yield per Acre', main = 'Livingston County Corn 2016')

# Calculate the 99.5% and 0.05% quantiles for each percentage of acres reported
q_upper_2016c <- apply(yhat_matrix, 2, quantile, 0.995)
q_lower_2016c <- apply(yhat_matrix, 2, quantile, 0.005)
lines(percent_numeric, q_upper_2016c, col = 'black', lty = 1, lwd = 5)
lines(percent_numeric, q_lower_2016c, col = 'black', lty = 1, lwd = 5)

# Plot Interquartile range
iqr = q_upper - q_lower
matplot(percent_reported * 100, iqr, type = 'l', ylab = 'IQR',
        xlab = 'Percent of Acres Reported', main = 'Livingston County Corn 2016')

################################################################################
# use same methods as above for soy yields for Mclean County 2016

# Use Livingston County for bootsrapping
Mclean_soy <- data |> filter(cropYear == 2016, commodityCode == 81, FIPS == 17113) |> select(yield, acres)

# Set the percentage of yields and acres reported
percent_reported <- seq(0.05, 1, 0.005)
percent_numeric <- percent_reported * 100

# Set the number of bootstrap samples to use
n_boot <- 1000

# Create empty matrix to store yhat values
yhat_matrix_soy <- matrix(NA, nrow = n_boot, ncol = length(percent_reported))
colnames(yhat_matrix_soy) <- paste0(percent_reported * 100, "%")

tic()
for (i in 1:n_boot) {
  # Randomly sample to get random order of farms reported
  rows <- sample(nrow(Mclean_soy), size = nrow(Mclean_soy), replace = FALSE)
  df <- Mclean_soy[rows,]
  
  # Calculate county yield per acre of farms reported
  sum_acres <- cumsum(df$acres)
  sum_yield <- cumsum(df$yield)
  production <- df$yield * df$acres
  sum_production <- cumsum(production)
  yhat <- sum_production / sum_acres
  
  # Get percentage of acres reported
  total_acres <- sum(df$acres)
  subset_percent <- sum_acres / total_acres
  
  # Find the index of the closest percentage in the subset_percent vector
  apick <- sapply(percent_reported, function(x) which.max(subset_percent >= x))
  
  # Extract yhat values at the specific indices
  yhat_sub <- yhat[apick]
  
  # Add yhat values to matrix
  yhat_matrix_soy[i,] <- yhat_sub
}
toc()

# Graph trajectories for yield per acre by percentage of acres reported
matplot(percent_numeric, t(yhat_matrix_soy), type = 'l', xlab = 'Percent of Acres Reported',
        ylab = 'Yield per Acre', main = 'Mclean County Soy 2016')

# Calculate the 99.5% and 0.05% quantiles for each percentage of acres reported
q_upper_soy <- apply(yhat_matrix_soy, 2, quantile, 0.995)
q_lower_soy <- apply(yhat_matrix_soy, 2, quantile, 0.005)
lines(percent_numeric, q_upper_soy, col = 'black', lty = 1, lwd = 5)
lines(percent_numeric, q_lower_soy, col = 'black', lty = 1, lwd = 5)

# Plot Interquartile range
iqr_soy = q_upper_soy - q_lower_soy
matplot(percent_reported * 100, iqr_soy, type = 'l', ylab = 'IQR',
        xlab = 'Percent of Acres Reported', main = 'Mclean County Soy 2016')
abline(v = which.min(iqr_soy >= 1), col = 'red')

################################################################################

################################################################################
# Create Plots for both counties for years 2008, 2012, 2016

# Livingston Corn 2008
Livingston_corn_2008 <- data |> filter(cropYear == 2008, commodityCode == 41, FIPS == 17105) |> select(yield, acres)

# Create empty matrix to store yhat values
yhat_matrix_2008c <- matrix(NA, nrow = n_boot, ncol = length(percent_reported))
colnames(yhat_matrix_2008c) <- paste0(percent_reported * 100, "%")

tic()
for (i in 1:n_boot) {
  # Randomly sample to get random order of farms reported
  rows <- sample(nrow(Livingston_corn_2008), size = nrow(Livingston_corn_2008), replace = FALSE)
  df <- Livingston_corn_2008[rows,]
  
  # Calculate county yield per acre of farms reported
  sum_acres <- cumsum(df$acres)
  sum_yield <- cumsum(df$yield)
  production <- df$yield * df$acres
  sum_production <- cumsum(production)
  yhat <- sum_production / sum_acres
  
  # Get percentage of acres reported
  total_acres <- sum(df$acres)
  subset_percent <- sum_acres / total_acres
  
  # Find the index of the closest percentage in the subset_percent vector
  apick <- sapply(percent_reported, function(x) which.max(subset_percent >= x))
  
  # Extract yhat values at the specific indices
  yhat_sub <- yhat[apick]
  
  # Add yhat values to matrix
  yhat_matrix_2008c[i,] <- yhat_sub
}
toc()

# Graph trajectories for yield per acre by percentage of acres reported
matplot(percent_numeric, t(yhat_matrix_2008c), type = 'l', xlab = 'Percent of Acres Reported',
        ylab = 'Yield per Acre', main = 'Livingston County Corn 2008')

# Calculate the 99.5% and 0.05% quantiles for each percentage of acres reported
q_upper_2008c <- apply(yhat_matrix_2008c, 2, quantile, 0.995)
q_lower_2008c <- apply(yhat_matrix_2008c, 2, quantile, 0.005)
lines(percent_numeric, q_upper_2008c, col = 'black', lty = 1, lwd = 5)
lines(percent_numeric, q_lower_2008c, col = 'black', lty = 1, lwd = 5)

################################################################################
# Livingston Corn 2012

Livingston_corn_2012 <- data |> filter(cropYear == 2012, commodityCode == 41, FIPS == 17105) |> select(yield, acres)

# Create empty matrix to store yhat values
yhat_matrix_2012c <- matrix(NA, nrow = n_boot, ncol = length(percent_reported))
colnames(yhat_matrix_2012c) <- paste0(percent_reported * 100, "%")

tic()
for (i in 1:n_boot) {
  # Randomly sample to get random order of farms reported
  rows <- sample(nrow(Livingston_corn_2012), size = nrow(Livingston_corn_2012), replace = FALSE)
  df <- Livingston_corn_2012[rows,]
  
  # Calculate county yield per acre of farms reported
  sum_acres <- cumsum(df$acres)
  sum_yield <- cumsum(df$yield)
  production <- df$yield * df$acres
  sum_production <- cumsum(production)
  yhat <- sum_production / sum_acres
  
  # Get percentage of acres reported
  total_acres <- sum(df$acres)
  subset_percent <- sum_acres / total_acres
  
  # Find the index of the closest percentage in the subset_percent vector
  apick <- sapply(percent_reported, function(x) which.max(subset_percent >= x))
  
  # Extract yhat values at the specific indices
  yhat_sub <- yhat[apick]
  
  # Add yhat values to matrix
  yhat_matrix_2012c[i,] <- yhat_sub
}
toc()

# Graph trajectories for yield per acre by percentage of acres reported
matplot(percent_numeric, t(yhat_matrix_2012c), type = 'l', xlab = 'Percent of Acres Reported',
        ylab = 'Yield per Acre', main = 'Livingston County Corn 2012')

# Calculate the 99.5% and 0.05% quantiles for each percentage of acres reported
q_upper_2012c <- apply(yhat_matrix_2012c, 2, quantile, 0.995)
q_lower_2012c <- apply(yhat_matrix_2012c, 2, quantile, 0.005)
lines(percent_numeric, q_upper_2012c, col = 'black', lty = 1, lwd = 5)
lines(percent_numeric, q_lower_2012c, col = 'black', lty = 1, lwd = 5)

################################################################################
# Create IQR plots for Livingston Corn years 2008, 2012, 2016

iqr_2008c = q_upper_2008c - q_lower_2008c
matplot(percent_reported * 100, iqr_2008c, type = 'l', ylab = 'IQR',
        xlab = 'Percent of Acres Reported', main = 'Livingston County Corn 2008')
#abline(v =iqr_2008c[which.min(iqr_2008c >= 2)[1]]  , col = 'red')

iqr_2012c = q_upper_2012c - q_lower_2012c
matplot(percent_reported * 100, iqr_2012c, type = 'l', ylab = 'IQR',
        xlab = 'Percent of Acres Reported', main = 'Livingston County Corn 2012')

iqr_2016c = q_upper_2016c - q_lower_2016c
matplot(percent_reported * 100, iqr_2016c, type = 'l', ylab = 'IQR',
        xlab = 'Percent of Acres Reported', main = 'Livingston County Corn 2016')

################################################################################

# Mclean Soy 2008

Mclean_soy_2008 <- data |> filter(cropYear == 2008, commodityCode == 81, FIPS == 17113) |> select(yield, acres)

# Create empty matrix to store yhat values
yhat_matrix_2008s <- matrix(NA, nrow = n_boot, ncol = length(percent_reported))
colnames(yhat_matrix_2008s) <- paste0(percent_reported * 100, "%")

tic()
for (i in 1:n_boot) {
  # Randomly sample to get random order of farms reported
  rows <- sample(nrow(Mclean_soy_2008), size = nrow(Mclean_soy_2008), replace = FALSE)
  df <- Mclean_soy_2008[rows,]
  
  # Calculate county yield per acre of farms reported
  sum_acres <- cumsum(df$acres)
  sum_yield <- cumsum(df$yield)
  production <- df$yield * df$acres
  sum_production <- cumsum(production)
  yhat <- sum_production / sum_acres
  
  # Get percentage of acres reported
  total_acres <- sum(df$acres)
  subset_percent <- sum_acres / total_acres
  
  # Find the index of the closest percentage in the subset_percent vector
  apick <- sapply(percent_reported, function(x) which.max(subset_percent >= x))
  
  # Extract yhat values at the specific indices
  yhat_sub <- yhat[apick]
  
  # Add yhat values to matrix
  yhat_matrix_2008s[i,] <- yhat_sub
}
toc()

# Graph trajectories for yield per acre by percentage of acres reported
matplot(percent_numeric, t(yhat_matrix_2008s), type = 'l', xlab = 'Percent of Acres Reported',
        ylab = 'Yield per Acre', main = 'Mclean County Soy 2008')

# Calculate the 99.5% and 0.05% quantiles for each percentage of acres reported
q_upper_2008s <- apply(yhat_matrix_2008s, 2, quantile, 0.995)
q_lower_2008s <- apply(yhat_matrix_2008s, 2, quantile, 0.005)
lines(percent_numeric, q_upper_2008s, col = 'black', lty = 1, lwd = 5)
lines(percent_numeric, q_lower_2008s, col = 'black', lty = 1, lwd = 5)

################################################################################
# Mclean Soy 2012

Mclean_soy_2012 <- data |> filter(cropYear == 2012, commodityCode == 81, FIPS == 17113) |> select(yield, acres)

# Create empty matrix to store yhat values
yhat_matrix_2012s <- matrix(NA, nrow = n_boot, ncol = length(percent_reported))
colnames(yhat_matrix_2012s) <- paste0(percent_reported * 100, "%")

tic()
for (i in 1:n_boot) {
  # Randomly sample to get random order of farms reported
  rows <- sample(nrow(Mclean_soy_2012), size = nrow(Mclean_soy_2012), replace = FALSE)
  df <- Mclean_soy_2012[rows,]
  
  # Calculate county yield per acre of farms reported
  sum_acres <- cumsum(df$acres)
  sum_yield <- cumsum(df$yield)
  production <- df$yield * df$acres
  sum_production <- cumsum(production)
  yhat <- sum_production / sum_acres
  
  # Get percentage of acres reported
  total_acres <- sum(df$acres)
  subset_percent <- sum_acres / total_acres
  
  # Find the index of the closest percentage in the subset_percent vector
  apick <- sapply(percent_reported, function(x) which.max(subset_percent >= x))
  
  # Extract yhat values at the specific indices
  yhat_sub <- yhat[apick]
  
  # Add yhat values to matrix
  yhat_matrix_2012s[i,] <- yhat_sub
}
toc()

# Graph trajectories for yield per acre by percentage of acres reported
matplot(percent_numeric, t(yhat_matrix_2012s), type = 'l', xlab = 'Percent of Acres Reported',
        ylab = 'Yield per Acre', main = 'Mclean County Soy 2012')

# Calculate the 99.5% and 0.05% quantiles for each percentage of acres reported
q_upper_2012s <- apply(yhat_matrix_2012s, 2, quantile, 0.995)
q_lower_2012s <- apply(yhat_matrix_2012s, 2, quantile, 0.005)
lines(percent_numeric, q_upper_2012s, col = 'black', lty = 1, lwd = 5)
lines(percent_numeric, q_lower_2012s, col = 'black', lty = 1, lwd = 5)

################################################################################
# Mclean Soy 2016

Mclean_soy_2016 <- data |> filter(cropYear == 2016, commodityCode == 81, FIPS == 17113) |> select(yield, acres)

# Create empty matrix to store yhat values
yhat_matrix_2016s <- matrix(NA, nrow = n_boot, ncol = length(percent_reported))
colnames(yhat_matrix_2016s) <- paste0(percent_reported * 100, "%")

tic()
for (i in 1:n_boot) {
  # Randomly sample to get random order of farms reported
  rows <- sample(nrow(Mclean_soy_2016), size = nrow(Mclean_soy_2016), replace = FALSE)
  df <- Mclean_soy_2016[rows,]
  
  # Calculate county yield per acre of farms reported
  sum_acres <- cumsum(df$acres)
  sum_yield <- cumsum(df$yield)
  production <- df$yield * df$acres
  sum_production <- cumsum(production)
  yhat <- sum_production / sum_acres
  
  # Get percentage of acres reported
  total_acres <- sum(df$acres)
  subset_percent <- sum_acres / total_acres
  
  # Find the index of the closest percentage in the subset_percent vector
  apick <- sapply(percent_reported, function(x) which.max(subset_percent >= x))
  
  # Extract yhat values at the specific indices
  yhat_sub <- yhat[apick]
  
  # Add yhat values to matrix
  yhat_matrix_2016s[i,] <- yhat_sub
}
toc()

# Graph trajectories for yield per acre by percentage of acres reported
matplot(percent_numeric, t(yhat_matrix_2016s), type = 'l', xlab = 'Percent of Acres Reported',
        ylab = 'Yield per Acre', main = 'Mclean County Soy 2016')

# Calculate the 99.5% and 0.05% quantiles for each percentage of acres reported
q_upper_2016s <- apply(yhat_matrix_2016s, 2, quantile, 0.995)
q_lower_2016s <- apply(yhat_matrix_2016s, 2, quantile, 0.005)
lines(percent_numeric, q_upper_2016s, col = 'black', lty = 1, lwd = 5)
lines(percent_numeric, q_lower_2016s, col = 'black', lty = 1, lwd = 5)

################################################################################
# Create IQR plots for Mclean Soy years 2008, 2012, 2016

iqr_2008s = q_upper_2008s - q_lower_2008s
matplot(percent_reported * 100, iqr_2008s, type = 'l', ylab = 'IQR',
        xlab = 'Percent of Acres Reported', main = 'Mclean County Soy 2008')
#abline(v =iqr_2008c[which.min(iqr_2008c >= 2)[1]]  , col = 'red')

iqr_2012s = q_upper_2012s - q_lower_2012s
matplot(percent_reported * 100, iqr_2012s, type = 'l', ylab = 'IQR',
        xlab = 'Percent of Acres Reported', main = 'Mclean County Soy 2012')

iqr_2016s = q_upper_2016s - q_lower_2016s
matplot(percent_reported * 100, iqr_2016s, type = 'l', ylab = 'IQR',
        xlab = 'Percent of Acres Reported', main = 'Mclean County Soy 2016')
