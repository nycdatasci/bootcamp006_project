######In this file, we do the feature engineering by multiply DebtRatio with MonthlyIncome to get DebtAmt. Then drop MonthlyIncome and
#####DebtRatio.

setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Capstone/nycdsa-capstone')
data = read.csv('./data/cs-training-outlier-f10.csv', header =T)
test = read.csv('./data/cs-test-outlier-f10.csv', header = T)
names(data)


DebtAmt = data$DebtRatio * as.numeric(data$MonthlyIncome)
DebtAmt = test$DebtRatio * as.numeric(test$MonthlyIncome)

data = as.data.frame(cbind(data, DebtAmt))
data = data[,-c(6,7)]

test = as.data.frame(cbind(test, DebtAmt))
test = test[,-c(6,7)]

write.csv(data,'cs-training-outlier-debt-f09.csv')
write.csv(test,'cs-test-outlier-debt-f09.csv')
