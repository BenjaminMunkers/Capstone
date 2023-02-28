
#import data
df1 = read.csv("C:/Users/jammu/Desktop/EFIN 499/msuCapstone_countyYields/UnitYields.csv")
df1$yieldAcre = df1$yield/df1$acres
state18 = subset(df1, stateFips == 18)
state18 = subset(state18, commodityCode == 41)
state18 = subset(state18, cropYear == 2016)
state17 = subset(df1, stateFips ==17)
county18 = data.frame(unique(state18$countyFips))
county18$meanYield = 1
county18$minYield = 1
county18$maxYield = 1
county18$meanYA = 1
county18$minYA = 1
county18$maxYA = 1
count = 0
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
tempdf = subset(state18, countyFips == 13) #13 has one of the lowest number of 
#farms. Other options are 25, 97, 87, 43, and 29.
plot(x = tempdf$yield, y = tempdf$acres, main = "Yield vs. Acres", xlab = "Yield", 
     ylab = "Acres")
table(state18$countyFips)

#calculate potential total county yields
county18$totalYield = 1
county18$newMean = 1
count = 0
for(i in unique(state18$countyFips)){
  count = count + 1
  tempdf = subset(state18, countyFips == i)
  tempdf$totalYield = 1
  #something is going wrong here with calculating the total yield of bushels
  for(j in length(tempdf$yield)){
    tempdf$totalYield[j] = tempdf$yield[j]*tempdf$acres[j]
  }
  totalAcres = sum(tempdf$acres)
  county18$totalYield[count] = sum(tempdf$totalYield)
  #totalYield = county18$totalYield[count]
  county18$newMean[count] = county18$totalYield[count]/totalAcres
}
