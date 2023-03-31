# This script creates variables for the drought data that count the number of weeks in each drought level 

library(lubridate)
library(tidyverse)
library(dplyr)

drought.df = read.csv("C:/Users/Joseph/Desktop/EFIN 499/dm_export_19970201_20170201.csv")

# Convert the percentage values to number of weeks
#drought.df[,5:10] <- round(drought.df[,5:10] * 52 / 100)

# Convert MapDate to a date format
drought.df$MapDate <- ymd(drought.df$MapDate)

# Extract the year from MapDate
drought.df$year <- year(drought.df$MapDate)

# Aggregate the data to number of weeks each year that are in each drought level
drought_aggregated <- drought.df |>
  group_by(year,County) |>
  summarize(num_none = sum(None),
            num_d0 = sum(D0),
            num_d1 = sum(D1),
            num_d2 = sum(D2),
            num_d3 = sum(D3),
            num_d4 = sum(D4),
            total_weeks = sum(num_none, num_d0, num_d1, num_d2, num_d3, num_d4)) |>
  mutate(num_none = num_none / total_weeks * 52,
         num_d0 = num_d0 / total_weeks * 52,
         num_d1 = num_d1 / total_weeks * 52,
         num_d2 = num_d2 / total_weeks * 52,
         num_d3 = num_d3 / total_weeks * 52,
         num_d4 = num_d4 / total_weeks * 52,) |>
  ungroup()

# Print the result
print(drought_aggregated)
test = subset(drought.df, year == 2000 & County == "Adams County")
(sum(test$None) / sum(test$None + test$D0 + test$D1 + test$D2 + test$D3 + test$D4))*52
