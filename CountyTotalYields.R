
#Setting up a DF where we can see county yield by year.

# import data
df1 = read.csv("C:/Users/jammu/Desktop/EFIN 499/msuCapstone_countyYields/UnitYields.csv")
# Sets up the blank data frame to be used
years = unique(df1$cropYear)
years = sort(years)
# tempdf for the specified state so we don't cross state county fip codes
tempdf= subset(df1, stateFips == 18)
masterdf = data.frame(unique(tempdf$countyFips))
colnames(masterdf)[1] = "countyFips"
count = 1
# might be a better way to code this but this works for now
for(i in years){
  count = count + 1
  masterdf = cbind(masterdf, i)
  colnames(masterdf)[count] = i
  masterdf[count] = 0
}
# setup of iterating through the data to get county level total yields
counties = unique(tempdf$countyFips)
for(i in 1:length(years)){
  temp = subset(tempdf, cropYear == years[i])
  for(j in 1:length(counties)){
    newtemp = subset(temp, countyFips == counties[j])
    yield = sum(newtemp$yield * newtemp$acres)
    masterdf[j,i+1] = yield
  }
}
