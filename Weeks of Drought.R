# This script creates variables for the drought data that count the number of weeks
# in each drought level and merges with the county yields and weekly averaged drought variables

library(lubridate)
library(tidyverse)
library(dplyr)

drought.df = read.csv("C:/Users/Joseph/Desktop/EFIN 499/dm_export_19970201_20170201.csv")

# Convert the percentage values to number of weeks

# Convert MapDate to a date format
drought.df$MapDate <- ymd(drought.df$MapDate)

# Extract the year from MapDate
drought.df$year <- year(drought.df$MapDate)

# Aggregate the data to number of weeks each year that are in each drought level
drought_aggregated <- drought.df |>
  group_by(year,County) |>
  reframe(FIPS = FIPS,
            num_none = sum(None),
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
         num_d4 = num_d4 / total_weeks * 52)

# Print the result
print(drought_aggregated)
test = subset(drought.df, year == 2000 & County == "Adams County")
(sum(test$None) / sum(test$None + test$D0 + test$D1 + test$D2 + test$D3 + test$D4))*52

# check to make sure sum of weeks in drought equals 52
for (i in 1:nrow(drought_aggregated1)) {
  drought_aggregated1$weeksinyear[i] = drought_aggregated1$num_none[i]+ drought_aggregated1$num_d0[i]+ drought_aggregated1$num_d1[i]+ 
    drought_aggregated1$num_d2[i]+ drought_aggregated1$num_d3[i]+ drought_aggregated1$num_d4[i]
}
summary(drought_aggregated1$weeksinyear)

drought_aggregated1 = unique(drought_aggregated)

################################################################################

# merge with weekly averaged drought level data frame
library(tictoc)
colnames(drought_aggregated1)[1] = "cropYear"
tic()
merged_drought_data <- merge(average.drought.levels, drought_aggregated1, by = c("cropYear", "FIPS"))
toc()

################################################################################

# Merge drought variables with county yields data set

county_yield_data = read.csv("C:/Users/Joseph/Desktop/EFIN 499/county_merged_drought_data.csv")

tic()
county_yield_drought = merge(county_yield_data, merged_drought_data, by = c("cropYear", "FIPS"))
toc()

################################################################################

# trim down merged data set to only useful variables

county_yield_drought_clean = county_yield_drought |>
  select(cropYear, CountyName, StateCode, CountyCode, FIPS, commodityCode, yield, acres,
         avg.None.x, avg.D0.x, avg.D1.x, avg.D2.x, avg.D3.x, avg.D4.x, 
         num_none, num_d0, num_d1, num_d2, num_d3, num_d4)

# rename column names to more readable variables
colnames(county_yield_drought_clean)[9] = "Avg.percent.None"
colnames(county_yield_drought_clean)[10] = "Avg.percent.D0"
colnames(county_yield_drought_clean)[11] = "Avg.percent.D1"
colnames(county_yield_drought_clean)[12] = "Avg.percent.D2"
colnames(county_yield_drought_clean)[13] = "Avg.percent.D3"
colnames(county_yield_drought_clean)[14] = "Avg.percent.D4"
colnames(county_yield_drought_clean)[15] = "Weeks.at.None"
colnames(county_yield_drought_clean)[16] = "Weeks.at.D0"
colnames(county_yield_drought_clean)[17] = "Weeks.at.D1"
colnames(county_yield_drought_clean)[18] = "Weeks.at.D2"
colnames(county_yield_drought_clean)[19] = "Weeks.at.D3"
colnames(county_yield_drought_clean)[20] = "Weeks.at.D4"

# save new dataframe

write.csv(county_yield_drought_clean, "C:/Users/Joseph/Desktop/EFIN 499/county_yield_drought_clean.csv")
