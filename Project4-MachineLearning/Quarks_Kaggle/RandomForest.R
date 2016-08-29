##333 Random Forest###############33333

library(caret)
library(randomForest)
library(pROC)
library(dplyr)
library(ROCR)



ctrl = trainControl(summaryFunction = twoClassSummary, #AMS_Summary before
                    method = 'LGOCV',
                    verboseIter = T,
                    savePredictions = T,
                    classProbs = T)

Sam_train_remove_imputed = Sam_train_remove_imputed[ ,-23 ]
Label_sam =dfTrain[rownames(Sam_train_remove_imputed),33] #label b s
Weight_sam = dfTrain[rownames(Sam_train_remove_imputed),32]

##########################################################3

# names(Sam_train_remove_imputed)

mtryValues = c(5,6,7, 8,9, 10, 12)
set.seed(0)

Sam_rf = train(x = Sam_train_remove_imputed,  y = Label_sam,
               method = "rf",
               weights = Weight_sam,
               ntree = 1000,
               tuneGrid = data.frame(.mtry = mtryValues),
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)

Sam_rf

################################################################
summary(Sam_rf)

names(Sam_rf)

confusionMatrix(data = Sam_rf$pred$pred,
                reference = Sam_rf$pred$obs,
                positive = "s")

# rf2008 <- merge(Sam_rf$pred,  Sam_rf$bestTune)

sam_predict_train_prob= predict(Sam_rf,Sam_train_remove_imputed,type = "prob") 
View(sam_predict_train_prob[ 2])# prob

sam_predict_train= predict(Sam_rf,Sam_train_remove_imputed) 
sam_predict_train


#----------------------------------------------------------------
#RORC

# Sam ROC Curves

library(randomForest)
#predictTrain = predict(Sam_rf$finalModel, Sam_train_remove_imputed, type = "prob")[,2]
predictTrain = Sam_rf$finalModel$votes[,2]
ROCRpred= prediction(predictTrain,Label_sam)
ROCRpred=performance(ROCRpred,"tpr","fpr")
plot(ROCRpred, colorize = TRUE, print.cutoffs.at= seq(0,1,0.1),
     text.adj = c(-0.2, 1.7) )

Sam_train_rf.roc = roc(response = Label_sam,
                       predictor = predictTrain)

Sam_train_rf.roc
plot(Sam_train_rf.roc, print.thres=TRUE)


#--------------------------------------------------------------

# joShua ROC Curves

RF_Test_Joshua_Pred <- predict(Joshua_rf_AMS, newdata=Joshua_test_remove_imputed, 
                               type="prob")


predictTrain_Joshua = Joshua_rf_AMS$finalModel$votes[,2]
ROCRpred_Joshua= prediction(predictTrain_Joshua,Label_Joshua)
ROCRpred_Joshua = performance(ROCRpred_Joshua,"tpr","fpr")
plot(ROCRpred_Joshua, colorize = TRUE, print.cutoffs.at= seq(0,1,0.1),
     text.adj = c(-0.2, 1.7) )

Joshua_train_rf.roc = roc(response = Label_Joshua,
                          predictor = predictTrain_Joshua)

Joshua_train_rf.roc
plot(Joshua_train_rf.roc, print.thres=TRUE)


#-------------------------------------------------------------

Sam_rf_ROC <- roc(response = Sam_rf$pred$obs,
                  predictor = Sam_rf$pred$s )
Sam_rf_ROC

plot(Sam_rf_ROC)
plot(Sam_rf)

Sam_RF_Var_Imp <- varImp(Sam_rf, scale = FALSE)
plot(Sam_RF_Var_Imp)
Sam_RF_Var_Imp


#--------------------------------------------------------------------------------

predict_Train_Sam = Sam_rf$finalModel$votes[,2]
# names(predict_Train_Sam) = substring(names(predict_Train_Sam),2,)

predict_Train_Charles = jet23.rf$finalModel$votes[,2]
names(predict_Train_Charles) = rownames(subset(train, PRI_jet_num > 1))

predict_Train_Joshua = Joshua_rf_AMS$finalModel$votes[,2]
# names(predict_Train_Joshua) = substring(names(predict_Train_Joshua),2,)

# head(predict_Train_Joshua)
# head(Joshua_rf_AMS$finalModel$votes[,2])

train_all = c(predict_Train_Joshua,predict_Train_Sam,predict_Train_Charles)
train_all = as.data.frame(train_all)
#train_all_df = cbind.data.frame(names(train_all), train_all)
train_all$ID = rownames(train_all)
train_all.sorted = arrange(train_all,as.numeric(ID))
train_all.roc = prediction(train_all.sorted$train_all, dfTrain$Label)
train_all.roctab = performance(train_all.roc, "tpr", "fpr")

plot(train_all.roctab, colorize = TRUE, print.cutoffs.at= seq(0,1,0.1),
     text.adj = c(-0.2, 1.7) )



#---------------------------------------------------------------------


train_rf.roc = roc(response = dfTrain$Label,
                   predictor = train_all.sorted$train_all)

auc1 = auc(train_rf.roc)
auc1

plot(train_rf.roc, print.thres=TRUE)



#------------------------------------------------------------------------------

#Test Data

# predict_test_charles =  predict(jet23.rf,Chales_test_remove_imputed,type = "prob") 


names(Sam_test_remove_imputed)
Sam_test_remove_imputed = Sam_test_remove_imputed[, -23]

names(Joshua_test_remove_imputed)
Joshua_test_remove_imputed = Joshua_test_remove_imputed[, -19]

names(Chales_test_remove_imputed)
Chales_test_remove_imputed = Chales_test_remove_imputed[, -31]

threshold <- 0.38

RF_Test_Sam_Pred <- predict(Sam_rf, newdata=Sam_test_remove_imputed, type="prob")

RF_Test_Joshua_Pred <- predict(Joshua_rf_AMS, newdata=Joshua_test_remove_imputed, 
                               type="prob")

############## Refix original table #####
library(dplyr)
Chales_test_remove_imputed = dfTest %>% filter(PRI_jet_num > 1)
Chales_test_remove_imputed$PRI_jet_num = as.factor(Chales_test_remove_imputed$PRI_jet_num)
Chales_test_remove_imputed$DER_mass_MMC = Chales_test_remove_imputed$DER_mass_MMC
Chales_test_remove_imputed$Weight = rep(0, 160128)
Chales_test_remove_imputed$EventId = (dfTest %>% filter(PRI_jet_num > 1))$EventId
# Labels Reversed by accident. because of trained model.
############################

RF_Test_Charles_Pred <- predict(jet23.rf, newdata=Chales_test_remove_imputed, 
                                type="prob")

test_all = rbind(as.data.frame(RF_Test_Sam_Pred),
                 as.data.frame(RF_Test_Joshua_Pred),
                 as.data.frame(RF_Test_Charles_Pred)
)



predicted <- rep("b",550000)
predicted[test_all[,2]>=threshold] <- "s"
weightRank = rank(test_all[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "gbm_submission.csv", row.names=FALSE) # gbm_shrink_ntrees_500_submission.csv


table(test_all$s>0.36)

#----------------------------------------------------------------------



#gbm

# threshold <- 0.002
library(pROC)


gbm_sam_TestPred = predict(Sam_gbm, newdata = Sam_test_remove_imputed, type = "prob")
gbm_sam_TrainPred = predict(Sam_gbm, newdata = Sam_train_remove_imputed, type = "prob")

labels = ifelse(Label_sam == 's', 1, 0)
auc = roc(labels, gbm_sam_TrainPred[,2])
plot(auc, print.thres = T)

plot(varImp(Sam_gbm))

# predicted <- rep("b",550000)
# predicted[gbmTestPred[,2]>=threshold] <- "s"
# weightRank = rank(gbmTestPred[,2], ties.method= "random")

