
# Intermediate model
rm(list = ls())
main_df = read.csv("C:/Users/jammu/Desktop/EFIN 499/final_merged_data.csv")
library(tidyverse)
state18 = subset(main_df, StateCode == 18)
count = 0
# gets total county yields for state 18
for(i in unique(state18$cropYear)){
  for(j in unique(state18$CountyCode)){
    count = count + 1
    countyLevel = subset(state18, CountyCode == j & cropYear == i)
    countyYield = sum(countyLevel$yield * countyLevel$acres)/sum(countyLevel$acres)
    if(count == 1){
      TYdf = data.frame(cropYear = c(i), CountyCode = c(j), Yield = c(countyYield), None = countyLevel$avg.None[1], 
                        D0 = countyLevel$avg.D0[1], D1 = countyLevel$avg.D1[1], D2 = countyLevel$avg.D2[1], D3 = countyLevel$avg.D3[1]
                        ,D4 = countyLevel$avg.D4[1])
    }
    else{
      TYdf = TYdf %>% 
        add_row(cropYear = c(i), CountyCode = c(j), Yield = c(countyYield), None = countyLevel$avg.None[1], 
                D0 = countyLevel$avg.D0[1], D1 = countyLevel$avg.D1[1], D2 = countyLevel$avg.D2[1], D3 = countyLevel$avg.D3[1]
                ,D4 = countyLevel$avg.D4[1])
    }
  }
}
trainSet = subset(TYdf, CountyCode == 41)
lm()
