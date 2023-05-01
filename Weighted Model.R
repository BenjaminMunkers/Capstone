library(tidyverse)
library(tictoc)
library(matrixStats)
library(ggplot2)
library(dplyr)

library(patchwork)
library(gridExtra)
# Read in data
main_df <- read.csv("C:/Users/Joseph/Desktop/EFIN 499/county_merged_drought_data.csv")

# Set the confidence level
conf_level <- 0.99

# Create an empty dataframe to store the results
weighted_results_df <- data.frame(Crop = integer(),
                         Year = integer(),
                         County = integer(),
                         Percent_Reported = integer(),
                         Farms_reported = integer(),
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
        mean_yield <- weighted.mean(sampled_data$yield, sampled_data$acres)
        sd_yield <- sqrt(weightedVar(sampled_data$yield, sampled_data$acres))
        # Calculate the confidence interval based on the t-distribution
        t_value <- qt(conf_level, df = n - 1)
        me <- t_value * (sd_yield / sqrt(n))
        # Append the results as a row to the data frame
        weighted_results_df <- rbind(weighted_results_df, data.frame(Crop = crop,
                                                   Year = year,
                                                   County = county,
                                                   Percent_Reported = i,
                                                   Farms_reported = n,
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
# 12396.81 sec elapsed after reverting to yield per acre

# Add county names to data frame
county_names <- main_df |> select(FIPS, CountyName) |> unique()
names(county_names)[1] <- "County"
weighted_results_df <- merge(weighted_results_df, county_names, by = "County")

# Save data frame
write.csv(weighted_results_df, "C:/Users/Joseph/Desktop/EFIN 499/weighted_mean_variance_ci.csv")
weighted_results_df <- read.csv("C:/Users/Joseph/Desktop/EFIN 499/weighted_mean_variance_ci.csv")

# Graph corn results
counties <- unique(weighted_results_df$County)[1:9]

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


################################################################################
# Create graphs for largest counties for soy and corn over 3 different years
# Largest Corn : Livingston County, FIPS = 17105
# Largest Soy: Mclean County, FIPS = 17113

# Read in data
CountyYields <- read_csv("C:/Users/Joseph/Desktop/EFIN 499/County_Yields")
data <- CountyYields |> select(cropYear, commodityCode, yield, acres, CountyName, FIPS)

# Plot average yield/acre for both counties to find years of interest to graph

# Largest Corn County
Livingston_ypa <- data |> filter(commodityCode == 41, FIPS == 17105) |>
  select(cropYear, yield) |> group_by(cropYear) |> summarise(avg_yield = mean(yield))
# Largest Soy County
Mclean_ypa <- data |> filter(commodityCode == 81, FIPS == 17113) |>
  select(cropYear, yield) |> group_by(cropYear) |> summarise(avg_yield = mean(yield))

# Combine the two datasets into one data frame with a new variable for county
Livingston_ypa$county <- 'Livingston'
Mclean_ypa$county <- 'Mclean'
df <- rbind(Livingston_ypa, Mclean_ypa)

# Create a ggplot
df |> group_by(county) |> ggplot(aes(x = cropYear, y = avg_yield, group = county)) +
  geom_line(aes(y = avg_yield, color = county)) +
  xlab('Year') +
  ylab('Average Farm Yield Per Acre') + ggtitle('Average Yield Per Acre')+
  scale_color_manual(values = c("Livingston" = "blue", "Mclean" = "red"),
                     labels = c("Livingston County (Corn)", "Mclean County (Soy)"))+
  geom_vline(xintercept = c(2008, 2012, 2016), linetype = "dashed")

################################################################################
# For years 2008, 2012, 2016: run weighted model for Livingston Corn and Mclean Soy

years <- c(2008, 2012, 2016)
county_codes <- c(17105, 17113)
crop_codes <- c(41,81)

# Create Graphs for Livingston Corn
weighted_results_df %>%
  filter(Crop == 41 & Year %in%years & County == 17105) %>%
  ggplot(aes(x = Percent_Reported, y = Mean_Yield)) + 
  geom_line(aes(y = Mean_Yield)) +
  geom_smooth(aes(y = Lower_CI, color = 'Lower 99% CI')) + 
  geom_smooth(aes(y = Upper_CI, color = 'Upper 99% CI'))+
  facet_wrap(~Year, nrow = 1, ncol = 3, scales = 'free')  +
  labs(x = "Percentage of yields reported", y = "Weighted Mean Yield", 
       title = "Weighted Mean Corn Yield and CI For Livingson County",
       color = "Confidence Interval") +
  theme(plot.title = element_text(hjust = 0.5))

# Create Graphs for Mclean Soy
weighted_results_df %>%
  filter(Crop == 81 & Year %in%years & County == 17113) %>%
  ggplot(aes(x = Percent_Reported, y = Mean_Yield)) + 
  geom_line(aes(y = Mean_Yield)) +
  geom_smooth(aes(y = Lower_CI, color = 'Lower 99% CI')) + 
  geom_smooth(aes(y = Upper_CI, color = 'Upper 99% CI'))+
  facet_wrap(~Year, nrow = 1, ncol = 3, scales = 'free')  +
  labs(x = "Percentage of yields reported", y = "Weighted Mean Yield", 
       title = "Weighted Mean Soy Yield and CI For Mclean County",
       color = "Confidence Interval") +
  theme(plot.title = element_text(hjust = 0.5))