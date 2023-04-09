
# Intermediate model
rm(list = ls())
library(lubridate)
library(tidyverse)
library(dplyr)
main_df = read.csv("C:/Users/jammu/Desktop/EFIN 499/final_merged_data.csv")

state18 = subset(main_df, StateCode == 18)
count = 0
# Doesn't work but trying to pipe to make the for loop easier to use
trialdf = main_df %>% 
  group_by(StateCode,CountyCode,commodityCode,cropYear) %>% 
  reframe(CountyYield = (yield*acres)/acres)
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
# Left out the lowest drought level of D0
lm(Yield ~ cropYear + avg.None + avg.D1 + avg.D2 + avg.D3 + avg.D4 + 
     num_none + num_d1 + num_d2 + num_d3 + num_d4, TYdf)
lm(Yield ~ cropYear + avg.None + avg.D1 + avg.D2 + avg.D3 + avg.D4, TYdf)
trainSet = subset(TYdf, TYdf$cropYear <2016)
trained_model = lm(Yield ~ cropYear + avg.None + avg.D1 + avg.D2 + avg.D3 + avg.D4 + 
                     num_none + num_d1 + num_d2 + num_d3 + num_d4, trainSet)
coelist = trained_model$coefficients
testSet = subset(TYdf,TYdf$cropYear == 2016)

