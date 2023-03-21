

rm = list(ls())
library(lubridate)
library(tidyverse)
# Import datasets
df1 = read.csv("C:/Users/jammu/Desktop/EFIN 499/msuCapstone_countyYields/UnitYields.csv")
df2 = read.csv("C:/Users/jammu/Desktop/Capstone/dm_export_19970201_20170201.csv")
# Sets up original data frame from John to have mergeable column names
df1$FIPS = df1$stateFips*1000 + df1$countyFips
colnames(df1)[1] = "Year"
# gets years
df2$ValidStart = mdy(df2$ValidStart)
df2$Year = year(df2$ValidStart)
# for loop to get averages of different drought levels
count = 0
for(i in unique(df2$Year)){
  temp = subset(df2, Year == i)
  for(j in unique(df2$FIPS)){
    df = subset(temp, FIPS == j)
    noDrought = mean(df$None)
    average0 = mean(df$D0)
    average1 = mean(df$D1)
    average2 = mean(df$D2)
    average3 = mean(df$D3)
    average4 = mean(df$D4)
    if(count == 0){
      count = 1+count
      bigdf = data.frame(Year = i, FIPS = j,None = noDrought, D0 = average0, 
                         D1 = average1, D2 = average2, D3 = average3, D4 = average4)
    } else{
      bigdf[nrow(bigdf)+1,] = list(i,j,noDrought,average0,average1,average2,average3,average4)
    }
  }  
}
# merges and then exports to a csv.
finaldf = merge(df1,bigdf, by=c("Year","FIPS"))
write.csv(finaldf, "C:/Users/jammu/Desktop/Capstone/ForLoopMerge.csv")
