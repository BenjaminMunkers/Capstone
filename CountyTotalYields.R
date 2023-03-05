
#Setting up a DF where we can see county yield by year.
rm(list = ls())
library(dplyr)
# import data
df1 = read.csv("C:/Users/jammu/Desktop/EFIN 499/msuCapstone_countyYields/UnitYields.csv")
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
