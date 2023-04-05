
# Intermediate model
rm(list = ls())
main_df = read.csv("C:/Users/jammu/Desktop/EFIN 499/final_merged_data.csv")
library(tidyverse)
state18 = subset(main_df, StateCode == 18)
count = 0
length(unique(state18$CountyCode))*length(unique(state18$cropYear))
length(unique(state18$num_none))
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
trainSet = subset(TYdf, CountyCode == 41)
TYdf = TYdf %>% 
  add_column(Weeks_None = unique(state18$num_none))
lm()
