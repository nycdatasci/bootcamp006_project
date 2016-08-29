# Read data and mark 999.0 as NAs
dfTrain <- read.csv('./data/training.csv', header=T)
dfTest <- read.csv('./data/test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA
testId = dfTest$Event

str(dfTrain)

weight <- dfTrain$Weight
labels <- dfTrain$Label

train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

sum(complete.cases(train)) # 68114

# Sample a part of data for model buliding
#train0 <- dfTrain[sample((nrow(dfTrain)), 10000), -c(1, 32)]
#train0$Label <- ifelse(train0$Label=='s', 1, 0)

################# logistic regression #################
logit.overall = glm(labels ~ .,
                    family = "binomial",
                    data = train)

#Residual plot for logistic regression with an added loess smoother; we would
#hope that, on average, the residual values are 0.
scatter.smooth(logit.overall$fit,
               residuals(logit.overall, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Admission Data")
abline(h = 0, lty = 2)

library(car)
influencePlot(logit.overall) #Can still inspect the influence plot.
summary(logit.overall) #Investigating the overall fit of the model.


###################### prediction with logistic regression ###############
predict(logit.overall, test, type = "response")

#Converting the fitted probabilities to binary:
label.predicted = round(logit.overall$fitted.values)

#Comparing the true values to the predicted values:
table(truth = labels, prediction = label.predicted)
