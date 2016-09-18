setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Capstone/nycdsa-capstone')
data = read.csv('./data/cs-training-outlier-f10.csv', header =T)
names(data)
library(dplyr)

new = data %>% filter(MonthlyIncome != 0)%>%group_by(NumberOfDependents) %>% summarise(avg=median(MonthlyIncome,na.rm=T))
names(new)
DepdRisk = 1:150000
for(i in 1:150000){
  if(is.na(data$MonthlyIncome[i])){
     DepdRisk[i] = NA
  }else{
    if(data$MonthlyIncome[i] == 0){
      DepdRisk[i] = data$NumberOfDependents[i]/(new[new$NumberOfDependents==data$NumberOfDependents[i],]$avg)
    }else{
      DepdRisk[i] = data$NumberOfDependents[i]/as.numeric(data$MonthlyIncome[i])
    }
  }
}

data = as.data.frame(cbind(data, DepdRisk))

data = data[,-c(7,12)]

write.csv(data,'cs-training-depdrisk-f09.csv')

setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Capstone/nycdsa-capstone')
test = read.csv('./data/cs-test-outlier-f10.csv', header = T)

new = test %>% filter(MonthlyIncome != 0)%>%group_by(NumberOfDependents) %>% summarise(avg=median(MonthlyIncome,na.rm=T))
names(new)
DepdRisk = 1:101503
for(i in 1:101503){
  if(is.na(test$MonthlyIncome[i])){
    DepdRisk[i] = NA
  }else{
    if(test$MonthlyIncome[i] == 0){
      DepdRisk[i] = test$NumberOfDependents[i]/(new[new$NumberOfDependents==test$NumberOfDependents[i],]$avg)
    }else{
      DepdRisk[i] = test$NumberOfDependents[i]/as.numeric(test$MonthlyIncome[i])
    }
  }
}
test = as.data.frame(cbind(test, DepdRisk))

test = test[,-c(7,12)]

write.csv(data,'cs-test-depdrisk-f09.csv')
