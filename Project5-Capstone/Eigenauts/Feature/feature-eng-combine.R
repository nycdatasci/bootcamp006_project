setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Capstone/nycdsa-capstone')
data = read.csv('./data/cs-training-outlier-f10.csv', header =T)

AgeRisk=1:150000

for(i in 1:150000){
  if(data$age[i]<29){
    AgeRisk[i]= 1-637/850
  }else{
    if(data$age[i]<39){
      AgeRisk[i]=1-654/850
    }else{
      if(data$age[i]<49){
        AgeRisk[i]=1-675/850
      }else{
        if(data$age[i]<59){
          AgeRisk[i]=1-697/850
        }else{
          if(data$age[i]<69){
            AgeRisk[i]=1-722/850
          }else{
            AgeRisk[i]=1-747/850
          }
        }
      }
    }
  }
}


r=c(0.105,0.089,0.119)

sum = sum(r)

w1 = r[1]/sum
w2 = r[2]/sum
w3 = r[3]/sum

default_time = w1 * data$NumberOfTime3059DaysPastDueNotWorse +w2 * data$NumberOfTime6089DaysPastDueNotWorse + w3 * data$NumberOfTimes90DaysLate
DebtAmt = data$DebtRatio * as.numeric(data$MonthlyIncome)

new = data %>% filter(MonthlyIncome != 0)%>%group_by(NumberOfDependents) %>% summarise(avg=median(MonthlyIncome,na.rm=T))

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

data = as.data.frame(cbind(data,AgeRisk,default_time,DebtAmt,DepdRisk))
data = data[,-c(4,5,6,7,9,11,12)]

write.csv(data,'cs-training-combine-f07.csv')





test = read.csv('./data/cs-test-outlier-f10.csv')

AgeRisk=1:101503

for(i in 1:101503){
  if(test$age[i]<29){
    AgeRisk[i]= 1-637/850
  }else{
    if(test$age[i]<39){
      AgeRisk[i]=1-654/850
    }else{
      if(test$age[i]<49){
        AgeRisk[i]=1-675/850
      }else{
        if(test$age[i]<59){
          AgeRisk[i]=1-697/850
        }else{
          if(test$age[i]<69){
            AgeRisk[i]=1-722/850
          }else{
            AgeRisk[i]=1-747/850
          }
        }
      }
    }
  }
}


default_time = w1 * test$NumberOfTime3059DaysPastDueNotWorse +w2 * test$NumberOfTime6089DaysPastDueNotWorse + w3 * test$NumberOfTimes90DaysLate
DebtAmt = test$DebtRatio * as.numeric(test$MonthlyIncome)
new = test %>% filter(MonthlyIncome != 0)%>%group_by(NumberOfDependents) %>% summarise(avg=median(MonthlyIncome,na.rm=T))

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
test = as.data.frame(cbind(test,AgeRisk,default_time,DebtAmt,DepdRisk))
test = test[,-c(4,5,7,9,11,12)]
write.csv(test,'cs-test-combine-f07.csv')
