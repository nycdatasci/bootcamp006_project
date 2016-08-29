#################### KAGGLE PROJECT RANDOM FOREST CODE ##############################

dfTrain <- read.csv('./data/training.csv', header=T)
dfTest <- read.csv('./data/test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA

library(randomForest)
library(ipred)
library(caret)
library(pROC)
library(gbm)
library(plyr)
library(inTrees)
library(RANN)


source('/Users/sharanduggal/Documents/Datascience/Kaggle_Jumpstart/helper.R')


############################################################################ PARAMETER TESTING 
############################################################################ PARAMETER TESTING 
############################################################################ PARAMETER TESTING 
############################################################################ PARAMETER TESTING 
############################################################################ PARAMETER TESTING 
############################################################################ PARAMETER TESTING 

samp = sample(1:77000, 5000)
dfTrain.jet = dfTrain.jet1[samp, ]
dfTest.jet = dfTrain.jet1[-samp, ]
dfTest.jet = dfTest.jet[5001:10000, ]

testId = dfTest.jet$EventId
str(dfTrain.jet)

weight <- dfTrain.jet$Weight
labels <- dfTrain.jet$Label
test.labels <- dfTest.jet$Label


train <- dfTrain.jet[, -c(1, 24,25)]
test <- dfTest.jet[,-c(1,24,25)]
str(train)


rfGrid <-  expand.grid(mtry = c(5,7))

ctrl = trainControl(method = "repeatedcv",number = 5, 
                    summaryFunction = AMS_summary, returnResamp = "all")#, classProbs = TRUE)

?trainControl
set.seed(0)
m_rf = train(x=train, y=labels, 
             method="rf", weights=weight, ntrees = 1000, importance = TRUE, keep.forest = TRUE,
             verbose=TRUE, trControl=ctrl, metric="AMS",  preProcess = "knnImpute", tuneGrid = rfGrid)

#maxnodes = 10, 

summary(m_rf)
plot(m_rf)
names(m_rf)
m_rf$finalModel
m_rf$bestTune
m_rf$times
m_rf$results
varImp(m_rf)

rfTrainPred <- predict(m_rf, newdata=train, type="prob")

summary(rfTrainPred)
names(rfTrainPred)
### ROC Curve ###
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, rfTrainPred[,2])
plot(auc, print.thres=TRUE)
table(rfTrainPred[ , 2]>=0.5, labels)
threshold <- 0.157

### Test Predict ###
rfTestPred <- predict(m_rf, newdata=test, type="prob")
test.labels = ifelse(test.labels=='s', 1, 0)
table(rfTestPred[ , 2]>=0.55, test.labels)


######################################################################################################  FILE 0
######################################################################################################  FILE 0
######################################################################################################  FILE 0
######################################################################################################  FILE 0
######################################################################################################  FILE 0
######################################################################################################  FILE 0

dfTrain <- read.csv('./data/training.csv', header=T)
dfTest <- read.csv('./data/test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA

testId = dfTest.jet0$EventId
weight <- dfTrain.jet0$Weight
labels <- dfTrain.jet0$Label

str(dfTrain.jet0)

train <- dfTrain.jet0[, -c(1, 21,22)]
test <- dfTest.jet0[,-1]
str(test)

rfGrid <-  expand.grid(mtry = c(5,6,7))

ctrl = trainControl(method = "repeatedcv",number = 5,
                    summaryFunction = AMS_summary, returnResamp = "all")

set.seed(0)
m_rf0 = train(x=train, y=labels, 
              method="rf", weights=weight, ntrees = 1000, importance = TRUE, keep.forest = TRUE,
              verbose=TRUE, trControl=ctrl, metric="AMS", preProcess = "medianImpute", tuneGrid = rfGrid)

?randomForest
plot(m_rf0)
m_rf0$finalModel
m_rf0$bestTune
m_rf0$times
varImp(m_rf0)

rfTrainPred0 <- predict(m_rf0, newdata=train, type="prob")

### ROC Curve ###
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, rfTrainPred0[,2])
plot(auc, print.thres=TRUE)
table(rfTrainPred0[ , 2]>=0.35, labels)
threshold = 0.35

### Test Predict ###
rfTestPred0 <- predict(m_rf0, newdata=test, type="prob")

predicted <- rep("b",nrow(test))
predicted[rfTestPred0[,2]>=threshold] <- "s"

file.a = data.frame(EventId = testId, Class = predicted, Rankcolumn = rfTestPred0[,2])

######################################################################################################  FILE 1
######################################################################################################  FILE 1
######################################################################################################  FILE 1
######################################################################################################  FILE 1
######################################################################################################  FILE 1
######################################################################################################  FILE 1

testId = dfTest.jet1$EventId
weight <- dfTrain.jet1$Weight
labels <- dfTrain.jet1$Label

str(dfTrain.jet1)
train <- dfTrain.jet1[, -c(1, 24,25)]
test <- dfTest.jet1[,-1]
str(test)

rfGrid <-  expand.grid(mtry = c(5,6,7))

ctrl = trainControl(method = "repeatedcv",number = 5, 
                    summaryFunction = AMS_summary, returnResamp = "all")

set.seed(0)
m_rf1 = train(x=train, y=labels, 
              method="rf", weights=weight, ntrees = 1000, importance = TRUE, keep.forest = TRUE,
              verbose=TRUE, trControl=ctrl, metric="AMS", preProcess = "medianImpute", tuneGrid = rfGrid)

plot(m_rf1)
names(m_rf1)
m_rf1$finalModel
m_rf1$bestTune
m_rf1$times
varImp(m_rf1)

rfTrainPred1 <- predict(m_rf1, newdata=train, type="prob")

summary(rfTrainPred1)
names(rfTrainPred1)

### ROC Curve ###
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, rfTrainPred1[,2])
plot(auc, print.thres=TRUE)
table(rfTrainPred1[ , 2]>=0.5, labels)
threshold = 0.5

### Test Predict ###
rfTestPred1 <- predict(m_rf1, newdata=test, type="prob")


predicted <- rep("b",nrow(test))
predicted[rfTestPred1[,2]>=threshold] <- "s"
file.b = data.frame(EventId = testId, Class = predicted, Rankcolumn = rfTestPred1[,2])


######################################################################################################  FILE 2
######################################################################################################  FILE 2
######################################################################################################  FILE 2
######################################################################################################  FILE 2
######################################################################################################  FILE 2
######################################################################################################  FILE 2


testId = dfTest.jet2$EventId
weight <- dfTrain.jet2$Weight
labels <- dfTrain.jet2$Label

str(dfTrain.jet2)
train <- dfTrain.jet2[, -c(1, 31,32)]
test <- dfTest.jet2[,-1]
str(train)

rfGrid <-  expand.grid(mtry = c(5,6,7))

ctrl = trainControl(method = "repeatedcv",number = 5, 
                    summaryFunction = AMS_summary, returnResamp = "all")

set.seed(0)
m_rf2 = train(x=train, y=labels, 
              method="rf", weights=weight, ntrees = 1000, importance = TRUE, keep.forest = TRUE,
              verbose=TRUE, trControl=ctrl, metric="AMS", preProcess = "medianImpute", tuneGrid = rfGrid)

plot(m_rf2)
names(m_rf2)
m_rf2$finalModel
m_rf2$bestTune
m_rf2$times
varImp(m_rf2)

rfTrainPred2 <- predict(m_rf2, newdata=train, type="prob")

summary(rfTrainPred2)
names(rfTrainPred2)
### ROC Curve ###
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, rfTrainPred2[,2])
plot(auc, print.thres=TRUE)
table(rfTrainPred2[ , 2]>=0.5, labels)
threshold = 0.5

### Test Predict ###
rfTestPred2 <- predict(m_rf2, newdata=test, type="prob")


predicted <- rep("b",nrow(test))
predicted[rfTestPred2[,2]>=threshold] <- "s"
file.c = data.frame(EventId = testId, Class = predicted, Rankcolumn = rfTestPred2[,2])
final.file = rbind(file.a, file.b, file.c)

weightRank = rank(final.file$Rankcolumn, ties.method= "random")

final.file = cbind(final.file, RankOrder = weightRank)

final.file.reduced = final.file[ , -3]
# write.csv(final.file, "rf_submission.csv", row.names=FALSE)

write.csv(final.file.reduced, "rf_submission_split.csv", row.names=FALSE)
write.csv(final.file, "rf_submission_probabilities.csv", row.names=FALSE)



######################################################################################################  FULL MODEL
######################################################################################################  FULL MODEL
######################################################################################################  FULL MODEL
######################################################################################################  FULL MODEL
######################################################################################################  FULL MODEL
######################################################################################################  FULL MODEL

dfTrain <- read.csv('./data/training.csv', header=T)
dfTest <- read.csv('./data/test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA

testId = dfTest$EventId
weight <- dfTrain$Weight
labels <- dfTrain$Label

str(dfTest)
train <- dfTrain[, -c(1, 32,33)]
test <- dfTest[,-1]
str(train)

rfGrid <-  expand.grid(mtry = 7)

ctrl = trainControl(method = "repeatedcv",number = 5, 
                    summaryFunction = AMS_summary, returnResamp = "all")

set.seed(0)
m_rf = train(x=train, y=labels, 
             method="rf", weights=weight, ntrees = 500, importance = TRUE, keep.forest = TRUE,
             verbose=TRUE, trControl=ctrl, metric="AMS", preProcess = "bagImpute", tuneGrid = rfGrid)

plot(m_rf)
names(m_rf)
m_rf$finalModel
m_rf$bestTune
m_rf$times
varImp(m_rf)

rfTrainPred <- predict(m_rf, newdata=train, type="prob")

summary(rfTrainPred)
names(rfTrainPred)
### ROC Curve ###
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, rfTrainPred[,2])
plot(auc, print.thres=TRUE)
table(rfTrainPred[ , 2]>=0.5, labels)
threshold = 0.5

### Test Predict ###
rfTestPred <- predict(m_rf, newdata=test, type="prob")

predicted <- rep("b",550000)
predicted[rfTestPred[,2]>=threshold] <- "s"
weightRank = rank(rfTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "rf_fullfile_submission.csv", row.names=FALSE)

submission_probs = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted, Probability = rfTestPred[,2])
write.csv(submission_probs, "rf_fullfile_submission_probs.csv", row.names=FALSE)

trainingid = dfTrain$EventId
training_probs = data.frame(EventId = trainingid, Class = dfTrain$Label, Probability = rfTrainPred[,2])
write.csv(training_probs, "rf_fullfile_training_probs.csv", row.names=FALSE)

