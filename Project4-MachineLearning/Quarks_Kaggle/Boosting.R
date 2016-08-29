
######################################
## GBM, Boosted Log (Bernoulli) ######
######################################

names(Sam_train_remove_imputed)

Sam_train_remove_imputed = Sam_train_remove_imputed[ ,-23 ]
Label_sam =dfTrain[rownames(Sam_train_remove_imputed),33] #label b s
Weight_sam = dfTrain[rownames(Sam_train_remove_imputed),32]
sam
source('./Kaggle_Jumpstart/helper.R')


ctrl_boosting = trainControl(summaryFunction = AMS_summary, #TwoClassSummary before
                    method = 'LGOCV',
                    savePredictions = T,
                    classProbs = T)

gbmGrid = expand.grid(interaction.depth = c(1,3,5), n.trees = 1000,
                      shrinkage = .001,
                      n.minobsinnode = c(100, 500, 2000))

Sam_gbm = train(x = Sam_train_remove_imputed, 
                y = Label_sam,
                 tuneGrid = gbmGrid,
                  method ="gbm",
                  weights = Weight_sam,
                  verbose = TRUE,
                  trControl = ctrl_boosting,
                  metric = "AMS")

Sam_gbm




library(pROC)


gbm_sam_TestPred = predict(Sam_gbm, newdata = Sam_test_remove_imputed, type = "prob")
gbm_sam_TrainPred = predict(Sam_gbm, newdata = Sam_train_remove_imputed, type = "prob")

labels = ifelse(Label_sam == 's', 1, 0)
auc = roc(labels, gbm_sam_TrainPred[,2])
plot(auc, print.thres = T)

plot(varImp(Sam_gbm))


#############################################################################################


Joshua_train_remove_imputed = Joshua_train_remove_imputed[ ,-19 ]
Label_Joshua =dfTrain[rownames(Joshua_train_remove_imputed),33] #label b s
Weight_Joshua = dfTrain[rownames(Joshua_train_remove_imputed),32]

source('helper.R')


ctrl_boosting = trainControl(summaryFunction = AMS_summary, #TwoClassSummary before
                             method = 'LGOCV',
                             savePredictions = T,
                             classProbs = T)

gbmGrid = expand.grid(interaction.depth = c(1,3,5), n.trees = 1000,
                      shrinkage = .001,
                      n.minobsinnode = c(100, 500, 2000))

Joshua_gbm = train(x = Joshua_train_remove_imputed, 
                   y = Label_Joshua,
                   tuneGrid = gbmGrid,
                   method ="gbm",
                   weights = Weight_Joshua,
                   verbose = TRUE,
                   trControl = ctrl_boosting,
                   metric = "AMS")

Joshua_gbm


gbm_Joshua_TestPred = predict(Joshua_gbm, newdata = Joshua_test_remove_imputed, type = "prob")
gbm_Joshua_TrainPred = predict(Joshua_gbm, newdata = Joshua_train_remove_imputed, type = "prob")

labels_joshua = ifelse(Label_Joshua == 's', 1, 0)
auc1 = roc(labels_joshua, gbm_Joshua_TrainPred[,2])
plot(auc1, print.thres = T)

plot(varImp(Joshua_gbm))


