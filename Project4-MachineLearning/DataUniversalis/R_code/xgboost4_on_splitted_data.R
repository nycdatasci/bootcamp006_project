library(randomForest)
library(caret)
library(doMC)
library(pROC)
library(xgboost)
registerDoMC(cores = 4)


env = "macbook"
#env = "gce"
#env = "aws"

if (env == "macbook") {
    base_dir = "/Users/sundeepblue/Bootcamp/week7/higgs-boson-kaggle/"
} else if (env == "gce") {
    base_dir = "/home/sundeepblue/higgs-boson-kaggle/"
} else {
    base_dir = "/home/ubuntu/higgs-boson-kaggle/"
}


setwd(base_dir)
source('helper.R')


dfTest = read.csv(paste0(base_dir, 'data/test.csv'), header=T)

which_jet_number = "0"
print(paste0("Training split ", which_jet_number))
dfTrain = read.csv(paste0(base_dir, 'data/splitted/subset0.csv'), header=T)
weight <- dfTrain$Weight
labels <- dfTrain$Label
train <- dfTrain[, -c(1,22,23,24)]
test <- dfTest[dfTest$PRI_jet_num == 0, -1]
test_eventId = dfTest[dfTest$PRI_jet_num == 0,]$EventId
# 
# ================================= launch model training, get best tune ===============================
set.seed(1)
ctrl = trainControl(method = "repeatedcv", verboseIter=T, number = 5, summaryFunction = AMS_summary)
#gbmGrid <-  expand.grid(nrounds=c(20, 60, 150), max_depth=c(10, 15, 20), eta=c(0.001, 0.05), colsample_bytree=0.7, gamma=5, min_child_weight=0.1)

# for split 0, learn very slowly is good!, depth=10 is good enough for slow learner
#gbmGrid <-  expand.grid(nrounds=c(250), max_depth=c(7, 10), eta=c(0.001), colsample_bytree=0.7, gamma=c(1, 5), min_child_weight=0.1)
gbmGrid <-  expand.grid(nrounds=c(50, 80, 150), max_depth=c(9), eta=c(0.1), colsample_bytree=0.7, gamma=c(5), min_child_weight=0.1)
set.seed(7)
begin_time = Sys.time()
print(paste0("Start training time: ", begin_time))
m_xgb = train(x=train, y=labels, method="xgbTree", weights=weight, tuneGrid=gbmGrid, verbose=2, trControl=ctrl, metric="AMS")
end_time = Sys.time()
print(paste0("Training finished at: ", end_time))
print(paste0("Training takes: ", end_time - begin_time))

best_model_tune_params = m_xgb$bestTune
fmtp = best_model_tune_params
file_name = paste0("_nrounds=", fmtp$nrounds, ",max_depth=", fmtp$max_depth, ",eta=", fmtp$eta, ",colsample_bytree=", fmtp$colsample_bytree, ",gamma=", fmtp$gamma,
                   ",min_child_weight=", fmtp$min_child_weight)

# ================= save models to images ====================================
jpeg(paste0("images/model_paras_", which_jet_number, file_name, ".jpg"), width=800, height=600)
plot(m_xgb)
dev.off()

gbmTrainPred <- predict(m_xgb, newdata=train, type="prob")
new_labels <- ifelse(labels=='s', 1, 0)
auc = roc(new_labels, gbmTrainPred[,2])

jpeg(paste0("images/AUC_for_split_", which_jet_number, file_name, ".jpg"), width=800, height=600)
plot(auc, print.thres=TRUE, main=file_name)
dev.off()


# # =================================================================================================
# # =================================================================================================

which_jet_number = "1"
dfTrain = read.csv(paste0(base_dir, 'data/splitted/subset1.csv'), header=T)
weight <- dfTrain$Weight
labels <- dfTrain$Label
train <- dfTrain[, -c(1,25,26,27)]
test <- dfTest[dfTest$PRI_jet_num == 1, -1]
test_eventId = dfTest[dfTest$PRI_jet_num == 1,]$EventId

# ================================= launch model training, get best tune ===============================
set.seed(1)
ctrl = trainControl(method = "repeatedcv", verboseIter=T, number = 10, summaryFunction = AMS_summary)
#gbmGrid <-  expand.grid(nrounds=c(20, 60, 150), max_depth=c(10, 15, 20), eta=c(0.001, 0.05), colsample_bytree=0.7, gamma=5, min_child_weight=0.1)
# for split 1, I need a quick learner
#gbmGrid <-  expand.grid(nrounds=c(200, 300), max_depth=c(10), eta=c(0.05), colsample_bytree=0.7, gamma=5, min_child_weight=0.1)
gbmGrid <-  expand.grid(nrounds=c(200, 250), max_depth=c(8, 10, 12), eta=c(0.02, 0.05, 0.15), colsample_bytree=0.7, gamma=5, min_child_weight=0.1)

set.seed(7)
begin_time = Sys.time()
print(paste0("Start training time: ", begin_time))
m_xgb = train(x=train, y=labels, method="xgbTree", weights=weight, tuneGrid=gbmGrid, verbose=2, trControl=ctrl, metric="AMS")
end_time = Sys.time()
print(paste0("Training finished at: ", end_time))
print(paste0("Training takes: ", end_time - begin_time))

best_model_tune_params = m_xgb$bestTune
fmtp = best_model_tune_params
file_name = paste0("_nrounds=", fmtp$nrounds, ",max_depth=", fmtp$max_depth, ",eta=", fmtp$eta, ",colsample_bytree=", fmtp$colsample_bytree, ",gamma=", fmtp$gamma, 
                   ",min_child_weight=", fmtp$min_child_weight)

# ================= save models to images ====================================
jpeg(paste0("images/model_paras_", which_jet_number, file_name, ".jpg"), width=800, height=600)
plot(m_xgb)
dev.off()

gbmTrainPred <- predict(m_xgb, newdata=train, type="prob")
new_labels <- ifelse(labels=='s', 1, 0)
auc = roc(new_labels, gbmTrainPred[,2])

jpeg(paste0("images/AUC_for_split_", which_jet_number, file_name, ".jpg"), width=800, height=600)
plot(auc, print.thres=TRUE, main=file_name)
dev.off()


# =================================================================================================
# =================================================================================================


which_jet_number = "23"

dfTrain = read.csv(paste0(base_dir, 'data/splitted/subset23.csv'), header=T)
weight <- dfTrain$Weight
labels <- dfTrain$Label
train <- dfTrain[, -c(1,32,33,34)]
test <- dfTest[dfTest$PRI_jet_num == 2 | dfTest$PRI_jet_num == 3, -1]
test_eventId = dfTest[dfTest$PRI_jet_num == 2 | dfTest$PRI_jet_num == 3,]$EventId

# ================================= launch model training, get best tune ===============================
set.seed(1)
ctrl = trainControl(method = "repeatedcv", verboseIter=T, number = 10, summaryFunction = AMS_summary)
#gbmGrid <-  expand.grid(nrounds=c(20, 60, 150), max_depth=c(10, 15, 20), eta=c(0.001, 0.05), colsample_bytree=0.7, gamma=5, min_child_weight=0.1)
gbmGrid <-  expand.grid(nrounds=c(200, 400), max_depth=c(20), eta=c(0.001, 0.05), colsample_bytree=0.7, gamma=5, min_child_weight=0.1)
#gbmGrid <-  expand.grid(nrounds=c(200, 250), max_depth=c(10, 20), eta=c(0.001, 0.003), colsample_bytree=0.7, gamma=5, min_chi
set.seed(7)
begin_time = Sys.time()
print(paste0("Start training time: ", begin_time))
m_xgb = train(x=train, y=labels, method="xgbTree", weights=weight, tuneGrid=gbmGrid, verbose=2, trControl=ctrl, metric="AMS")
end_time = Sys.time()
print(paste0("Training finished at: ", end_time))
print(paste0("Training takes: ", end_time - begin_time))

best_model_tune_params = m_xgb$bestTune
fmtp = best_model_tune_params
file_name = paste0("_nrounds=", fmtp$nrounds, ",max_depth=", fmtp$max_depth, ",eta=", fmtp$eta, ",colsample_bytree=", fmtp$colsample_bytree, ",gamma=", fmtp$gamma, 
                   ",min_child_weight=", fmtp$min_child_weight)

# ================= save models to images ====================================
jpeg(paste0("images/model_paras_", which_jet_number, file_name, ".jpg"), width=800, height=600)
plot(m_xgb)
dev.off()

gbmTrainPred <- predict(m_xgb, newdata=train, type="prob")
new_labels <- ifelse(labels=='s', 1, 0)
auc = roc(new_labels, gbmTrainPred[,2])

jpeg(paste0("images/AUC_for_split_", which_jet_number, file_name, ".jpg"), width=800, height=600)
plot(auc, print.thres=TRUE, main=file_name)
dev.off()


#=================== handle submissions ======================================
#need to combine multiple splits together into a large one, and generate the final submission.
generate_submission = F
if (generate_submission == TRUE) {
    threshold <- 0.245
    gbmTestPred <- predict(m_xgb, newdata=test, type="prob")
    num_test = nrow(test)
    predicted <- rep("b", num_test)
    predicted[gbmTestPred[,2] >= threshold] = "s"
    weightRank = rank(gbmTestPred[,2], ties.method= "random")

    submission = data.frame(EventId = test_eventId, RankOrder = weightRank, Class = predicted)
    output_file_name = paste0("submission_for_split_", which_jet_number, file_name, ".csv")
    write.csv(submission, output_file_name, row.names=FALSE)
}
