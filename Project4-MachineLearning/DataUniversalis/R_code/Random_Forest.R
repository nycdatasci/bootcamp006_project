######################
###  Main Function ###
######################


#############################
####  Preprocessing data ####
#############################
#setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Machine_Learning')
# Read data and mark 999.0 as NAs
dfTrain <- read.csv('training.csv',header = T)
dfTest <- read.csv('test.csv',header = T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA
testId = dfTest$EventId

dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)

str(dfTrain)

weight <- dfTrain$Weight
labels <- dfTrain$Label

train_with_label <- dfTrain[, -c(1,32)]
train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

sum(complete.cases(train)) # 68114


######## Only about a quarter of the dataset are complete cases, so how do we impute those missing values?
######## Most tree models can handle missing values automatically. Check the following links to see how random forest and gbm
######## handle missing values in the training set.
######## http://stackoverflow.com/questions/14718648/r-gbm-handling-of-missing-values
######## http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#missing1


##########################
####  Build gbm model ####
##########################

library(caret)
###### Check the documentation for the details of those functions
###### https://cran.r-project.org/web/packages/caret/caret.pdf

####### Library used for parallel processing
####### Check details here: http://topepo.github.io/caret/parallel.html
####### Windows users please follow the instruction on stackoverflow: http://stackoverflow.com/a/24655923
library(doMC)
registerDoMC(cores = 4)

# Load our customized metric function.
source('./Kaggle_Jumpstart/helper.R')

###### Setup a 5 fold cross-validation and use AMS as the metric
###### AMS_summary function defined in helper.R
###### Check details here: http://topepo.github.io/caret/training.html#control
ctrl = trainControl(method = "repeatedcv",number = 5,
                    summaryFunction = AMS_summary)

###### Setup grid search parameters. 
###### The more candidates you provide, the longer it will take to train the model.
gbmGrid <-  expand.grid(interaction.depth = c(4,6,8), n.trees =(2:8)*100,
                        shrinkage = c(0.01, 0.005, 0.001),
                        n.minobsinnode = c(100, 500, 2000))

###### Train the gbm model
###### If you want to use the gbmGrid you defined above, you could simply set tuneGrid = gbmGrid in the train function.
m_gbm = train(x=train, y=labels, 
              method="gbm", weights=weight, 
              verbose=TRUE, trControl=ctrl, metric="AMS")

###### You can think of this model as a logistic regression. For a logistic regression, we need to find the best threshold for ROC and AUC.
###### Check the definition of ROC and AUC here: 
###### http://thestatsgeek.com/2014/05/05/area-under-the-roc-curve-assessing-discrimination-in-logistic-regression/
gbmTrainPred <- predict(m_gbm, newdata=train, type="prob")

library(pROC)
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, gbmTrainPred[,2])
plot(auc, print.thres=TRUE)

######## From the graph, we can tell the best threshold is 0.002
threshold <- 0.002

gbmTestPred <- predict(m_gbm, newdata=test, type="prob")

predicted <- rep("b",550000)
predicted[gbmTestPred[,2]>=threshold] <- "s"
weightRank = rank(gbmTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "gbm_submission.csv", row.names=FALSE)

####################################
####  Build random forest model ####
####################################
library(randomForest)
###### The only thing you need to change is the name of method.
###### Check all the available algorithms by typing names(getModelInfo())
###### Check avaliable tuning parameters here: http://topepo.github.io/caret/modelList.html
rfGrid <-  expand.grid(mtry = c(3,4,5))

# compare results
results <- resamples(modellist)

# attemp.dftrain = dfTrain[sample(nrow(dfTrain),nrow(dfTrain)/10),]
# label_sample = attemp.dftrain$Label
# weight_sample = attemp.dftrain$Weight
# attemp = attemp.dftrain[,-c(1,32,33)]
# attemp2 = attemp.dftrain[,-c(1,32)]

df0 = read.csv('subset0.csv')
label_df0 = df0$Label
weight_df0 = df0$Weight
df0 = df0[,-c(1,20,21,22,23,24)]

df1 = read.csv('subset1.csv')
label_df1 = df1$Label
weight_df1 = df1$Weight
df1 = df1[,-c(1,25,26,27)]

df23 = read.csv('subset23.csv')
label_df23 = df23$Label
weight_df23 = df23$Weight
df23 = df23[,-c(1,32,33,34)]
df23$PRI_jet_num = as.factor(df23$PRI_jet_num)
####random forest for df0 ####

set.seed(0)
m_rf_df0 = train(x=df0, y=label_df0,
             method="rf", weights=weight_df0,
             verbose=TRUE, trControl=ctrl, metric="AMS", tuneGrid = rfGrid)
# Random Forest 
# 
# 99913 samples
# 21 predictor
# 2 classes: 'b', 's' 
# 
# No pre-processing
# Resampling: Cross-Validated (2 fold, repeated 1 times) 
# Summary of sample sizes: 49957, 49956 
# Resampling results across tuning parameters:
#   
#   mtry  AMS     
# 3     1.458663
# 6     1.431591
# 9     1.426653
# 
# AMS was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 3. 


####random forest for df1 ####
set.seed(1)
m_rf_df1 = train(x=df1, y=label_df1,
             method="rf", weights=weight_df1,
             verbose=TRUE, trControl=ctrl, metric="AMS", tuneGrid = rfGrid, ntree = 5000)
plot(m_rf_df1)
# 
# Random Forest 
# 
# 77544 samples
# 24 predictor
# 2 classes: 'b', 's' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 62036, 62035, 62035, 62035, 62035 
# Resampling results across tuning parameters:
#   
#   mtry  AMS      
# 3     0.6493734
# 6     0.6473204
# 9     0.6468350
# 
# AMS was used to select the optimal model using  the
# largest value.
# The final value used for the model was mtry = 3. 
####random forest for df23 ####
set.seed(2)
m_rf_df23 = train(x=df23, y=label_df23,
             method="rf", weights=weight_df23,
             verbose=TRUE, trControl=ctrl, metric="AMS", tuneGrid = rfGrid)

# Random Forest 
# 
# 36342 samples
# 31 predictor
# 2 classes: 'b', 's' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 29073, 29074, 29073, 29074, 29074 
# Resampling results across tuning parameters:
#   
#   mtry  AMS      
# 3     0.4752992
# 6     0.4701267
# 9     0.4643957
# 
# AMS was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 3. 



begin_time = Sys.time()
print(paste0("Start training time: ", begin_time))

#m_xgb = train(x=train, y=labels, method="xgbTree", weights=weight, tuneGrid=gbmGrid, verbose=2, trControl=ctrl, metric="AMS")
set.seed(7)
m_rf_df1_cur = train(x=df1, y=label_df1, method="rf", weights=weight_df1, tuneGrid=rfGrid,ntree = 1000, verbose=2, trControl=ctrl, metric="AMS")

end_time = Sys.time()
print(paste0("Training finished at: ", end_time))
print(paste0("Training takes: ", end_time - begin_time))

best_model_tune_params = m_rf_df1_cur$bestTune
fmtp = best_model_tune_params
file_name = paste0('mtry:',fmtp$mtry)

# ================= save models to images ====================================
jpeg_name = paste0("./model_paras_", "0", file_name, ".jpg")
jpeg(jpeg_name, width=800, height=600)
plot(m_rf_df1_cur)
dev.off()

rfTrainPred <- predict(m_rf_df1_cur, newdata=df1, type="prob")
new_labels <- ifelse(label_df1=='s', 1, 0)
auc = roc(new_labels, rfTrainPred[,2])

#pdf(paste0("./images/AUC_for_split_", "0", file_name, ".pdf"), width=800, height=600)
plot(auc, print.thres=TRUE, main=file_name)
#dev.off()



set.seed(7)
begin_time = Sys.time()
print(paste0("Start training time: ", begin_time))
#m_xgb = train(x=train, y=labels, method="xgbTree", weights=weight, tuneGrid=gbmGrid, verbose=2, trControl=ctrl, metric="AMS")
m_rf_df23_cur = train(x=df23, y=label_df23, method="rf", weights=weight_df23, tuneGrid=rfGrid, verbose=2, trControl=ctrl, metric="AMS")
end_time = Sys.time()
print(paste0("Training finished at: ", end_time))
print(paste0("Training takes: ", end_time - begin_time))

best_model_tune_params = m_rf_df23_cur$bestTune
fmtp = best_model_tune_params
file_name = paste0('mtry:',fmtp$mtry)

# ================= save models to images ====================================
jpeg_name = paste0("./model_paras_", "0", file_name, ".jpg")
jpeg(jpeg_name, width=800, height=600)
plot(m_rf_df23_cur)
dev.off()

rfTrainPred <- predict(m_rf_df23_cur, newdata=df23, type="prob")

new_labels <- ifelse(label_df23=='s', 1, 0)
auc = roc(new_labels, rfTrainPred[,2])

#pdf(paste0("./images/AUC_for_split_", "0", file_name, ".pdf"), width=800, height=600)
plot(auc, print.thres=TRUE, main=file_name)
#dev.off()

######################
#####   Xgboost  #####
######################

###### Check the source code here: https://github.com/dmlc/xgboost/tree/master/demo/kaggle-higgs
###### The slides I found quite useful to understand xgboost: https://homes.cs.washington.edu/~tqchen/pdf/BoostedTree.pdf
###### The offical paper of xgboost: https://arxiv.org/pdf/1603.02754v2.pdf



####################################
#####   Where to go from here  #####
####################################


######## 1. Feature engineering
######## It is a huge topic but you can get some idea from the winning solution: https://github.com/phunterlau/kaggle_higgs


######## 2. Missing values
######## Tree models like random forest or gbm could handle missing values automatically. So does xgboost.
######## What if you just want to build a simple logistic regression?

######## 3. Ensemble method
######## How to make you final model works better by ensembling couple models?
######## For a classification problem, a majority vote would be a straight-forward approach.
######## Check out this tutorial for a complete guide: http://mlwave.com/kaggle-ensembling-guide/


######## 4. Stacking method
######## This is a more advanced topic. Most of the winning solutions on Kaggle using two-layer stacking method.
######## Check out this tutorial http://machinelearningmastery.com/machine-learning-ensembles-with-r/

