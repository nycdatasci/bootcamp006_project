setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Capstone/nycdsa-capstone')
data = read.csv('./data/cs-training-outlier-f10.csv', header =T)

require(rms)
names(data)

logit.x1 = lrm(SeriousDlqin2yrs ~ NumberOfTime3059DaysPastDueNotWorse,data = data)
logit.x1


logit.x2 = lrm(SeriousDlqin2yrs ~ NumberOfTime6089DaysPastDueNotWorse,data = data)
logit.x2

logit.x3 = lrm(SeriousDlqin2yrs ~ NumberOfTimes90DaysLate,data = data)
logit.x3

r=c(0.105,0.089,0.119)

sum = sum(r)

w1 = r[1]/sum
w2 = r[2]/sum
w3 = r[3]/sum

default_time = w1 * data$NumberOfTime3059DaysPastDueNotWorse +w2 * data$NumberOfTime6089DaysPastDueNotWorse + w3 * data$NumberOfTimes90DaysLate
data = as.data.frame(cbind(data,default_time))
data = data[,-c(5,9,11)]
write.csv(data,'cs-training-outlier-weight-f08.csv')

test = read.csv('./data/cs-test-outlier-f10.csv')
default_time = w1 * test$NumberOfTime3059DaysPastDueNotWorse +w2 * test$NumberOfTime6089DaysPastDueNotWorse + w3 * test$NumberOfTimes90DaysLate
test = as.data.frame(cbind(test,default_time))
test = test[,-c(5,9,11)]
write.csv(test,'cs-test-outlier-weight-f08.csv')
