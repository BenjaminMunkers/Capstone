
#Setting up a DF where we can see county yield by year.
rm(list = ls())
library(dplyr)
library(tidyr)
# import data
df1 = read.csv("C:/Users/Joseph/Desktop/EFIN 499/msuCapstone_countyYields/msuCapstone_countyYields/UnitYields.csv")
# Gets the years of the data frame
years = unique(df1$cropYear)
years = sort(years)
# tempdf for the specified state and crop so we don't cross state county fip codes
tempdf= subset(df1, stateFips == 18)
tempdf = subset(tempdf, commodityCode == 81)
# sets up the data frame to be used
totalYields = data.frame(unique(tempdf$countyFips))
averageYields = totalYields
colnames(totalYields)[1] = "countyFips"
count = 1
# might be a better way to code this but this works for now
# sets up the blank data frames for yields and average yield
for(i in years){
  count = count + 1
  totalYields = cbind(totalYields, i)
  averageYields = cbind(averageYields, i)
  colnames(totalYields)[count] = i
  colnames(averageYields)[count] = i
  totalYields[count] = 0
  averageYields[count] = 0
}

# setup of iterating through the data to get county level total yields and average
counties = averageYields
for(i in 1:length(years)){
  temp = subset(tempdf, cropYear == years[i])
  #farms = temp %>% 
  #count(countyFips)
  for(j in 1:length(counties)){
    newtemp = subset(temp, countyFips == counties[j])
    yield = sum(newtemp$yield * newtemp$acres)
    totalYields[j,i+1] = yield
    averageYields[j,i+1] = yield/sum(newtemp$acres)
    #counties[j, i+1] = farms[j,2]
  }
}

# read in county names
county.names.df = read.csv("C:/Users/Joseph/Desktop/EFIN 499/msuCapstone_countyYields/CountyNames.csv")

# read in drought data 
drought.df = read.csv("C:/Users/Joseph/Desktop/EFIN 499/dm_export_19970201_20170201.csv")

# extract year from each entry
library(lubridate)
drought.df$date <- as.Date(drought.df$ValidStart)
drought.df$year <- year(drought.df$ValidStart)

# average weekly percentage of drought levels to yearly data
average.drought.levels <- 
  aggregate(
    cbind(drought.df$None, drought.df$D0, drought.df$D1, drought.df$D2, drought.df$D3, drought.df$D4), 
    list(drought.df$FIPS, drought.df$year),
            data = drought.df, FUN = mean)

# rename columns
colnames(average.drought.levels) = c('FIPS', 'Year', 'avg.None', 'avg.D0', 'avg.D1', 'avg.D2', 'avg.D3', 'avg.D4')

# merge yearly averaged data with main drought dataset
drought.aggregated.df = left_join(average.drought.levels, drought.df, multiple = "all")

write.csv(drought.aggregated.df, file = 'C:/Users/Joseph/Desktop/EFIN 499/aggregated_drought_data.csv')
