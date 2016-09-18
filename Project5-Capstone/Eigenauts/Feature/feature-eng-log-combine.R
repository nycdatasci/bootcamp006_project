setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Capstone/nycdsa-capstone')
data = read.csv('./data/cs-training-log-f10.csv', header =T)
data = data[,-1]
Debt = data$MonthlyIncome * data$DebtRatio
data = as.data.frame(cbind(data,Debt))
data = data[,-c(6,7)]

write.csv(data, './data/cs-training-log-combine-f09.csv')

test = read.csv('./data/cs-test-log-f10.csv', header =T)
test = test[,-1]
Debt = test$MonthlyIncome * test$DebtRatio
test = as.data.frame(cbind(test,Debt))

test = test[,-c(6,7)]
write.csv(data, './data/cs-test-log-combine-f09.csv')
