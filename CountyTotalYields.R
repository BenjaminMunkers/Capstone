
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

################################################################################

# read in county names
county.names.df = read.csv("C:/Users/Joseph/Desktop/EFIN 499/msuCapstone_countyYields/CountyNames.csv")

# read in drought data 
drought.df = read.csv("C:/Users/Joseph/Desktop/EFIN 499/dm_export_19970201_20170201.csv")

# extract year from each entry
library(lubridate)
drought.df$date <- as.Date(drought.df$ValidStart)
drought.df$cropYear <- year(drought.df$ValidStart)

# average weekly percentage of drought levels to yearly data
average.drought.levels <- 
  aggregate(
    cbind(drought.df$None, drought.df$D0, drought.df$D1, drought.df$D2, drought.df$D3, drought.df$D4), 
    list(drought.df$FIPS, drought.df$year),
            data = drought.df, FUN = mean)

# rename columns
colnames(average.drought.levels) = c('FIPS', 'cropYear', 'avg.None', 'avg.D0', 'avg.D1', 'avg.D2', 'avg.D3', 'avg.D4')


# merge yearly averaged data with main drought dataset
drought.aggregated.df = merge(average.drought.levels, drought.df, by = c('cropYear','FIPS'))

# export file
write.csv(drought.aggregated.df, file = 'C:/Users/Joseph/Desktop/EFIN 499/aggregated_drought_data.csv')

################################################################################

# add county names to unit yield dataframe
county_yield_data = read.csv("C:/Users/Joseph/Desktop/EFIN 499/msuCapstone_countyYields/msuCapstone_countyYields/UnitYields.csv")

county_name_code <- county.names.df %>%
  subset(county.names.df$StateCode == 17 | county.names.df$StateCode == 18)

county_name_code <- data.frame(county_name_code$CountyCode, county_name_code$CountyName, county_name_code$StateCode)
colnames(county_name_code)[1] <- "CountyCode"
colnames(county_name_code)[2] <- "CountyName"
colnames(county_name_code)[3] <- "StateCode"
colnames(county_yield_data)[4] <- "CountyCode"
colnames(county_yield_data)[3] <- "StateCode"

county_merged_data <- left_join(county_yield_data, county_name_code)
county_merged_data$CountyName <- paste(county_merged_data$CountyName, "County")

# create FIPS column with state and county FIPS together
county_merged_data$FIPS = (county_merged_data$StateCode*1000) + county_merged_data$CountyCode

#subset averaged drought data to only useful columns
drought.averaged <- drought.aggregated.df |> select(cropYear, FIPS, None, D0, D1, D2, D3, D4)

# merge averaged drought data with county merged data
county_merged_drought_data <- merge(county_merged_data, drought.averaged, by = c("cropYear", "FIPS"))
