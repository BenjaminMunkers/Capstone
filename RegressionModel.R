
# Intermediate model
rm(list = ls())
main_df = read.csv("C:/Users/jammu/Desktop/EFIN 499/merged_data.csv")
library(tidyverse)
state18 = subset(main_df, StateCode == 18)
count = 0
# gets total county yields for state 18
for(i in unique(state18$cropYear)){
  for(j in unique(state18$CountyCode)){
    count = count + 1
    countyLevel = subset(state18, CountyCode == j & cropYear == i)
    countyYield = sum(countyLevel$yield * countyLevel$acres)
    if(count == 1){
      TYdf = data.frame(cropYear = c(i), CountyCode = c(j), Yield = c(countyYield))
    }
    else{
      TYdf = TYdf %>% 
        add_row(cropYear = c(i), CountyCode = c(j), Yield = c(countyYield))
    }
  }
}
