
# Intermediate model
rm(list = ls())
library(tidyverse)
main_df = read.csv("C:/Users/jammu/Desktop/EFIN 499/final_merged_data.csv")

state18 = subset(main_df, StateCode == 18)
count = 0
# gets total county yields for state 18
for(i in unique(state18$cropYear)){
  for(j in unique(state18$CountyCode)){
    count = count + 1
    countyLevel = subset(state18, CountyCode == j & cropYear == i)
    countyYield = sum(countyLevel$yield * countyLevel$acres)/sum(countyLevel$acres)
    if(count == 1){
      TYdf = data.frame(cropYear = c(i), CountyCode = c(j), Yield = c(countyYield), countyLevel[1,11:22])
    }
    else{
      TYdf = TYdf %>% 
        add_row(cropYear = c(i), CountyCode = c(j), Yield = c(countyYield), countyLevel[1,11:22])
    }
  }
}
head(TYdf)
trainSet = subset(TYdf, CountyCode == 41)
# Left out the lowest drought level of D0
lm(Yield ~ cropYear + avg.None + avg.D1 + avg.D2 + avg.D3 + avg.D4 + 
     num_none + num_d1 + num_d2 + num_d3 + num_d4, trainSet)
lm(Yield ~ cropYear + avg.None + avg.D1 + avg.D2 + avg.D3 + avg.D4, trainSet)
