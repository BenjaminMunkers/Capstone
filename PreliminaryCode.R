
# import data
df1 = read.csv("C:/Users/jammu/Desktop/EFIN 499/msuCapstone_countyYields/UnitYields.csv")
# not needed as I think the yields are already in yield per acre
#df1$yieldAcre = df1$yield/df1$acres
# breaks the data into Indiana corn for 2016
state18 = subset(df1, stateFips == 18)
state18 = subset(state18, commodityCode == 41)
state18 = subset(state18, cropYear == 2016)
# Illinois variable taken out for time being
#state17 = subset(df1, stateFips ==17)
# county level data for Indiana
county18 = data.frame(unique(state18$countyFips))
county18$meanYield = 1
county18$minYield = 1
county18$maxYield = 1
county18$meanYA = 1
county18$minYA = 1
county18$maxYA = 1
count = 0
# gets mean min and max yields per acre for the counties
# YA variables are not need at this time if yields are already in per acre values
for(i in unique(state18$countyFips)){
  count = count + 1
  tempdf = subset(state18, countyFips == i)
  county18$meanYield[count] = mean(tempdf$yield)
  county18$maxYield[count] = max(tempdf$yield)
  county18$minYield[count] = min(tempdf$yield)
  county18$meanYA[count] = mean(tempdf$yieldAcre)
  county18$minYA[count] = min(tempdf$yieldAcre)
  county18$maxYA[count] = max(tempdf$yieldAcre)
}
tempdf = subset(state18, countyFips == 13) 
# 13 has one of the lowest number of farms. 
# Other options are 25, 97, 87, 43, and 29.
# plots yields and acres to see how and if they interact.
plot(x = tempdf$yield, y = tempdf$acres, main = "Yield vs. Acres", xlab = "Yield", 
     ylab = "Acres")
table(state18$countyFips)

#calculate potential total county yields and state yields
county18$totalYield = 1
county18$newMean = 1
county18$totalAcres = 1
count = 0
for(i in unique(state18$countyFips)){
  count = count + 1
  tempdf = subset(state18, countyFips == i)
  tempdf$totalYield = 1
  # calculates the total yields for each county.
  for(j in 1:length(tempdf$yield)){
    tempdf$totalYield[j] = tempdf$yield[j]*tempdf$acres[j]
  }
  # moves the total acres and yields into the county variable
  totalAcres = sum(tempdf$acres)
  county18$totalYield[count] = sum(tempdf$totalYield)
  county18$totalAcres[count] = totalAcres
  # gets the county mean weighted by the number of acres
  county18$newMean[count] = county18$totalYield[count]/totalAcres
}
# calculate state average
stateAve = sum(county18$totalYield)/sum(county18$totalAcres)
# currently have a higher average yield per acre than reported online
# 178.929 calculated vs. 173 reported online.