setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Capstone/nycdsa-capstone')
data = read.csv('./data/cs-training.csv', header =T)

Debt = data$MonthlyIncome * data$DebtRatio
data = as.data.frame(cbind(data,Debt))
data = data[,-c(6,7)]

data$RevolvingUtilizationOfUnsecuredLines = log(data$RevolvingUtilizationOfUnsecuredLines+1)
data$NumberOfTime30.59DaysPastDueNotWorse = log(data$NumberOfTime30.59DaysPastDueNotWorse+1)
data$Debt = log(data$Debt+1)
data$NumberOfOpenCreditLinesAndLoans = log(data$NumberOfOpenCreditLinesAndLoans+1)
data$NumberOfTimes90DaysLate = log(data$NumberOfTimes90DaysLate+1)
data$NumberOfTime60.89DaysPastDueNotWorse = log(data$NumberOfTime60.89DaysPastDueNotWorse+1)
data$NumberOfDependents = log(data$NumberOfDependents+1)
data$NumberRealEstateLoansOrLines = log(data$NumberRealEstateLoansOrLines+1)

write.csv(data,'./data/cs-training-combine-log-f09.csv')

test = read.csv('./data/cs-test.csv', header =T)

Debt = test$MonthlyIncome * test$DebtRatio
test = as.data.frame(cbind(test,Debt))
test = test[,-c(6,7)]

test$RevolvingUtilizationOfUnsecuredLines = log(test$RevolvingUtilizationOfUnsecuredLines+1)
test$NumberOfTime30.59DaysPastDueNotWorse = log(test$NumberOfTime30.59DaysPastDueNotWorse+1)
test$Debt = log(test$Debt+1)
test$NumberOfOpenCreditLinesAndLoans = log(test$NumberOfOpenCreditLinesAndLoans+1)
test$NumberOfTimes90DaysLate = log(test$NumberOfTimes90DaysLate+1)
test$NumberOfTime60.89DaysPastDueNotWorse = log(test$NumberOfTime60.89DaysPastDueNotWorse+1)
test$NumberOfDependents = log(test$NumberOfDependents+1)
test$NumberRealEstateLoansOrLines = log(test$NumberRealEstateLoansOrLines+1)

write.csv(data,'./data/cs-test-combine-log-f09.csv')