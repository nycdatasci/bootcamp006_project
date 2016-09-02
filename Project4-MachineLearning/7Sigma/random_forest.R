library(caret)

dfTrain = read.csv('./training.csv', header=T)
dfTest = read.csv('./test.csv', header=T)
dfTrain[dfTrain == -999.0] = NA
dfTest[dfTest == -999.0] = NA
testId = dfTest$EventId

dfTrain_repNA = dfTrain
dfTrain_repNA[is.na(dfTrain_repNA)] = 0
dfTest_repNA = dfTest
dfTest_repNA[is.na(dfTest_repNA)] = 0

train_subset = dfTrain_repNA[1:1000,]
test_subset = dfTest_repNA[1:1000,]

str(dfTrain)

weight = dfTrain_repNA$Weight
labels = dfTrain_repNA$Label

train = dfTrain[, -c(1,32,33)]
test = dfTest[,-1]

train_repNA = dfTrain_repNA[, -c(1,32,33)]
test_repNA = dfTest_repNA[,-1]

ctrl = trainControl(method
                    = "repeatedcv",number = 2, summaryFunction = AMS_summary)

rfGrid =  expand.grid(mtry = c(3,6,9))
m_rf = train(x=train_repNA, y=labels, method="rf", weights=weight, verbose=TRUE,
             trControl=ctrl, metric="AMS")
m_rf$finalModel
rfTestPred = predict(m_rf, newdata=test_repNA, type="prob")

predicted = rep("b",550000) 
predicted[rfTestPred[,2]>=threshold] = "s"
weightRank = rank(rfTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class =
                          predicted) 
write.csv(submission, "rf_submission.csv", row.names=FALSE)


vimp <- varImp(m_rf)
