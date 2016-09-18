# The 18 - 29 age group has an average credit score of 637.
# The 30 - 39 age group has an average score of 654.
# The 40 - 49 age group has an average score of 675.
# The 50 - 59 age group has an average score of 697.
# The 60 - 69 age group has an average score of 722.
# The 70 plus age group has an average score of 747.
# Agerisk = 1 - (CreditScore / 850)
setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Capstone/nycdsa-capstone')
data = read.csv('./data/cs-training-outlier-f10.csv', header =T)
test = read.csv('./data/cs-test-outlier-f10.csv', header = T)


AgeRisk=1:150000
AgeRisk=1:101503

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
data = data[,-4]
test = test[,-4]
data = as.data.frame(cbind(data,AgeRisk))
test = as.data.frame(cbind(test,AgeRisk))
write.csv(data,'cs-training-outlier-agerisk-f10.csv')
write.csv(test,'cs-test-outlier-agerisk-f10.csv')
