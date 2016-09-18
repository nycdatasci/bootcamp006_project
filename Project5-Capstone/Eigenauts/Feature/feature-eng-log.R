setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Capstone/nycdsa-capstone')
data = data = read.csv('./data/cs-training.csv', header =T)
names(data)
summary(data)

##By the summary, we can apply log transformation on RevolvingUtilizationOfUnsecuredLines,NumberOfTime30.59DaysPastDueNotWorse,
##DebtRatio, MonthlyIncome, NumberOfOpenCreditLinesAndLoans, NumberOfTimes90DaysLate, NumberRealEstateLoansOrLines, NumberOfTime60.89DaysPastDueNotWorse
##NumberOfDependents

data$RevolvingUtilizationOfUnsecuredLines = log(data$RevolvingUtilizationOfUnsecuredLines+1)
data$NumberOfTime30.59DaysPastDueNotWorse = log(data$NumberOfTime30.59DaysPastDueNotWorse+1)
data$DebtRatio = log(data$DebtRatio+1)
data$MonthlyIncome = log(data$MonthlyIncome+1)
data$NumberOfOpenCreditLinesAndLoans = log(data$NumberOfOpenCreditLinesAndLoans+1)
data$NumberOfTimes90DaysLate = log(data$NumberOfTimes90DaysLate+1)
data$NumberOfTime60.89DaysPastDueNotWorse = log(data$NumberOfTime60.89DaysPastDueNotWorse+1)
data$NumberOfDependents = log(data$NumberOfDependents+1)
data$NumberRealEstateLoansOrLines = log(data$NumberRealEstateLoansOrLines+1)
write.csv(data,'cs-training-log-f10.csv')

test = read.csv('./data/cs-test.csv', header =T)
summary(test)
test$RevolvingUtilizationOfUnsecuredLines = log(test$RevolvingUtilizationOfUnsecuredLines+1)
test$NumberOfTime30.59DaysPastDueNotWorse = log(test$NumberOfTime30.59DaysPastDueNotWorse+1)
test$DebtRatio = log(test$DebtRatio+1)
test$MonthlyIncome = log(test$MonthlyIncome+1)
test$NumberOfOpenCreditLinesAndLoans = log(test$NumberOfOpenCreditLinesAndLoans+1)
test$NumberOfTimes90DaysLate = log(test$NumberOfTimes90DaysLate+1)
test$NumberOfTime60.89DaysPastDueNotWorse = log(test$NumberOfTime60.89DaysPastDueNotWorse+1)
test$NumberOfDependents = log(test$NumberOfDependents+1)
test$NumberRealEstateLoansOrLines = log(test$NumberRealEstateLoansOrLines+1)

write.csv(test,'cs-test-log-f10.csv')