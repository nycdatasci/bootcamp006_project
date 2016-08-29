library(gbm)
library(caret)
library(doMC)
library(pROC)

###########GBM CODE APPLIED TO A SEPERATE DATA FRAMES#############
registerDoMC(cores = 4)
source('/Users/cholmes/Downloads/Kaggle_Jumpstart/helper.R')
ctrl = trainControl(method = "repeatedcv",number = 2,
                    summaryFunction = AMS_summary)

gbmGrid <-  expand.grid(interaction.depth = c(4,6,8), n.trees =(2:8)*100,
                        shrinkage = c(0.01, 0.005, 0.001),
                        n.minobsinnode = c(100, 500, 2000))

###### Training the gbm models
m_gbm_0 = train(x=df_0, y=labels_0, 
              method="gbm", weights = weight_0,
              verbose=TRUE, trControl=ctrl, metric="AMS")
m_gbm_1 = train(x=df_1, y=labels_1, 
                method="gbm", weights=weight_1, 
                verbose=TRUE, trControl=ctrl, metric="AMS")
m_gbm_2 = train(x=df_2, y=labels_2, 
                method="gbm", weights=weight_2, 
                verbose=TRUE, trControl=ctrl, metric="AMS")
m_gbm_3 = train(x=df_3, y=labels_3, 
                method="gbm", weights=weight_3, 
                verbose=TRUE, trControl=ctrl, metric="AMS")

gbmTrainPred_0 <- predict(m_gbm_0, newdata=df_0, type="prob")
gbmTrainPred_1 <- predict(m_gbm_1, newdata=df_1, type="prob")
gbmTrainPred_2 <- predict(m_gbm_2, newdata=df_2, type="prob")
gbmTrainPred_3 <- predict(m_gbm_3, newdata=df_3, type="prob")

#Setting up vectors for s's and b's
labels_0_n <- ifelse(labels_0=='s', 1, 0)
labels_1_n <- ifelse(labels_1=='s', 1, 0)
labels_2_n <- ifelse(labels_2=='s', 1, 0)
labels_3_n <- ifelse(labels_3=='s', 1, 0)

#Determining the ideal thresholds
auc_0 = roc(labels_0_n, gbmTrainPred_0[,2])
auc_1 = roc(labels_1_n, gbmTrainPred_1[,2])
auc_2 = roc(labels_2_n, gbmTrainPred_2[,2])
auc_3 = roc(labels_3_n, gbmTrainPred_3[,2])

plot(auc_0, print.thres=TRUE)
plot(auc_1, print.thres=TRUE)
plot(auc_2, print.thres=TRUE)
plot(auc_3, print.thres=TRUE)


threshold_0 <- 0.001
threshold_1 <- 0.002
threshold_2 <- 0.006
threshold_3 <- 0.005

#Making Predictions using gbm models
gbmTestPred_0 <- predict(m_gbm_0, newdata=df_0_test, type="prob")
gbmTestPred_1 <- predict(m_gbm_1, newdata=df_1_test, type="prob")
gbmTestPred_2 <- predict(m_gbm_2, newdata=df_2_test, type="prob")
gbmTestPred_3 <- predict(m_gbm_3, newdata=df_3_test, type="prob")

predicted_0 <- rep("b",nrow(df_0_test))
predicted_1 <- rep("b",nrow(df_1_test))
predicted_2 <- rep("b",nrow(df_2_test))
predicted_3 <- rep("b",nrow(df_3_test))

#Writing predictions to vector
predicted_0[gbmTestPred_0[,2]>=threshold_0] <- "s"
predicted_1[gbmTestPred_1[,2]>=threshold_1] <- "s"
predicted_2[gbmTestPred_2[,2]>=threshold_2] <- "s"
predicted_3[gbmTestPred_3[,2]>=threshold_3] <- "s"
predicted = c(predicted_0,predicted_1,predicted_2,predicted_3)

#Calulating weights and writing new submission
weighted = c(gbmTestPred_0[,2],gbmTestPred_1[,2],gbmTestPred_2[,2],gbmTestPred_3[,2])
weightRank = rank(weighted, ties.method= "random")
testId = c(testId_0, testId_1, testId_2, testId_3)
submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "/Users/cholmes/Desktop/gbm_submission.csv", row.names=FALSE)


###########GBM CODE APPLIED TO A SINGLE DATA FRAME#############
gbmGrid <-  expand.grid(interaction.depth = c(4,6,8), n.trees =(2:8)*100,
                        shrinkage = c(0.01, 0.005, 0.001),
                        n.minobsinnode = c(100, 500, 2000))

# Training the gbm model
m_gbm = train(x=train, y=labels,
              method="gbm", weights=weight, 
              verbose=TRUE, trControl=ctrl, metric="AMS")

#Making GBM predictions 
gbmTrainPred <- predict(m_gbm, newdata=train, type="prob")

#Determining the best threshold
labels_n <- ifelse(labels=='s', 1, 0)
auc = roc(labels_n, gbmTrainPred[,2])
plot(auc, print.thres=TRUE)
threshold <- 0.002

#Predicting and writing the gbm predictions
gbmTestPred <- predict(m_gbm, newdata=test, type="prob")
predicted <- rep("b",550000)
predicted[gbmTestPred[,2]>=threshold] <- "s"
weightRank = rank(gbmTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "gbm_submission_single.csv", row.names=FALSE)





