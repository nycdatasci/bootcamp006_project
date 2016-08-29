#########################
#####   Load Data   #####
#########################
train_jet_0_raw = read.csv('./Final_data/train_jet_0', header = T)
train_jet_1_raw = read.csv('./Final_data/train_jet_1', header = T)
train_jet_2_3_raw = read.csv('./Final_data/train_jet_2_3', header = T)
test_jet_0_raw = read.csv('./Final_data/test_jet_0', header = T)
test_jet_1_raw = read.csv('./Final_data/test_jet_1', header = T)
test_jet_2_3_raw = read.csv('./Final_data/test_jet_2_3', header = T)


train_jet_0 = train_jet_0_raw[,-1]
train_jet_1 = train_jet_1_raw[,-1]
train_jet_2_3 = train_jet_2_3_raw[,-1]

test_jet_0 = test_jet_0_raw[,-1]
test_jet_1 = test_jet_1_raw[,-1]
test_jet_2_3 = test_jet_2_3_raw[,-1]


######################
#####   Models   #####
######################
if (!require(gbm)) install.packages('gbm')
if (!require(caret)) install.packages('caret')
if (!require(doMC)) install.packages('doMC')
if (!require(pROC)) install.packages('pROC')
if (!require(caretEnsemble)) install.packages('caretEnsemble')
if (!require(mlbench)) install.packages('mlbench')
library(gbm)
library(caret)
library(doMC)
library(pROC)
library(caretEnsemble)
library(mlbench)
source('helper.R')

# calculate AMS score
# AMS = function(real,pred,weight)
calculateAMS = function(prediction, y, weight) {
  pred = ifelse(gbmTrainPred[,2] < 0.5, 'b', 's')
  input = data.frame(obs = y)
  input$obs = as.character(y)
  input$pred = as.character(pred)
  input$weights = weight
  return(AMS_summary(input))
}


#-------------------------------------------------
# Random Forest
#-------------------------------------------------
registerDoMC(cores = 32)

num = nrow(train_jet_1)
index = sample(nrow(train_jet_1), num)
toy_data1 = train_jet_1[index, ]

x = toy_data1[, 1:(ncol(toy_data1)-2)]
weight = toy_data1[, ncol(toy_data1)-1]
y = toy_data1[, ncol(toy_data1)]

ctrl = trainControl(method = "repeatedcv",number = 10,
                    summaryFunction = AMS_summary,
                    selectionFunction = 'oneSE')

rfGrid <-  expand.grid(mtry = c(2, 5),
                       maxdepth = c(4, 10, 25))

start = proc.time()
# train model
rf = train(x=x, y=y,
           method="rfRules", weights=weight,
           verbose=TRUE, trControl=ctrl,
           tuneGrid = rfGrid, metric="AMS")
rf$results

proc.time() - start

# prediction
# validation_index = setdiff(1:nrow(train_jet_0), index)
rf_pred <- predict(rf, newdata = train_jet_1, type="prob")

# plot ROC
labels <- ifelse(y == 's', 1, 0)
par(mfrow = c(1, 1))
auc = roc(labels, rf_pred[, 2])
plot(auc, print.thres = TRUE)


#-----------------Make submission

# model_jet_0 = models$rf2
# model_jet_1 = models$rf2
# model_jet_2_3 = models$rf2
#threshold = 0.002: 1.08,
#threshold = 0.085: 1.42,
#threshold = 0.15 : 1.67,
#threshold = 0.20 : 1.87,
#threshold = 0.215: 1.94,
#threshold = 0.25 : 2.07,
#threshold = 0.30 : 2.27,
#threshold = 0.50 : 2.94,
#threshold = 0.60 : 3.2,
#threshold = 0.65 : 3.27,
#threshold = 0.68 : 3.27,
#threshold = 0.70 : 3.28,
#threshold = 0.705: 3.27,
#threshold = 0.71 : 3.28,
#threshold = 0.73 : 3.24,
#threshold = 0.75 : 3.20,
#threshold = 0.80 : 3.0,
#threshold = 0.90 : 2.17,



subset(test_15,test_15$RankOrder == row_num)
nrow(subset(combined_output,combined_output$s > 0.1089255))/550000

threshold = 0.705
filename = 'SecretPipeline0.705.csv'

model1 = model_jet_0
data1 = test_jet_0
id1 = data.frame(EventId = test_jet_0_raw$EventId)

output1 = predict(model1, data1, type = 'prob')
prediction1 = rep('b', nrow(data1))
prediction1[output1[, 2] >= threshold ] = 's'


model2 = model_jet_1
data2 = test_jet_1
id2 = data.frame(EventId = test_jet_1_raw$EventId)

output2 = predict(model2, data2, type = 'prob')
prediction2 = rep('b', nrow(data2))
prediction2[output2[, 2] >= threshold ] = 's'


model3 = model_jet_2_3
data3 = test_jet_2_3
id3 = data.frame(EventId = test_jet_2_3_raw$EventId)

output3 = predict(model3, data3, type = 'prob')
prediction3 = rep('b', nrow(data3))
prediction3[output3[, 2] >= threshold ] = 's'


combined_output = rbind.data.frame(output1, output2, output3)
weightRank = rank(combined_output[, 2], ties.method= "random")
combined_id = rbind.data.frame(id1, id2, id3)
combined_prediction = c(prediction1, prediction2, prediction3)
submission = data.frame(EventId = combined_id, RankOrder = weightRank, Class = combined_prediction)

write.csv(submission, filename, row.names = F)

sum(combined_prediction == 's') / length(combined_prediction)

# variable importance
plot(varImp(model_jet_0))
plot(varImp(model_jet_1))
plot(varImp(model_jet_2_3))



#-------------------------------------------------------------
# Stochastic Grandient Boosting/Grandient Boosted Machine(gbm)
#-------------------------------------------------------------
less_train_jet_0 = read.csv('./Final_data/less_train_jet_0', header = T)
less_train_jet_1 = read.csv('./Final_data/less_train_jet_1', header = T)
less_train_jet_2_3 = read.csv('./Final_data/less_train_jet_2_3', header = T)
less_test_jet_0 = read.csv('Final_data/less_test_jet_0', header = T)
less_test_jet_1 = read.csv('Final_data/less_test_jet_1', header = T)
less_test_jet_2_3 = read.csv('Final_data/less_test_jet_2_3', header = T)


registerDoMC(cores = 36)
num = nrow(train_jet_0)
toy_data1 = train_jet_0[sample(nrow(train_jet_0), num), ]

x = toy_data1[, 1:(ncol(toy_data1)-2)]
weight = toy_data1[, ncol(toy_data1)-1]
y = toy_data1[, ncol(toy_data1)]

ctrl = trainControl(method = "repeatedcv",number = 5,
                    summaryFunction = AMS_summary,
                    selectionFunction = 'oneSE')

gbmGrid <-  expand.grid(interaction.depth = c(5), n.trees = 3000,
                        shrinkage = c(0.1),
                        n.minobsinnode = c(100))
start = proc.time()
# train model
m_gbm1 = train(x=x, y=y, 
              method="gbm", weights=weight, 
              verbose=FALSE, trControl=ctrl, 
              tuneGrid = gbmGrid, metric="AMS")
m_gbm$bestTune

# prediction
gbmPred1 = predict(m_gbm1, newdata=test_jet_1, type="prob")
write.csv(gbmPred1, 'gbm1.csv',row.names = F)

proc.time() - start


# # plot ROC 
# labels <- ifelse(y == 's', 1, 0)
# par(mfrow = c(1, 1))
# auc = roc(labels, gbmTrainPred[, 2])
# plot(auc, print.thres = TRUE)


#-----------------Make submission
output1 = read.csv('./gbm_predictions/gbm0.csv', header = T)
output2 = read.csv('./gbm_predictions/gbm1.csv', header = T)
output3 = read.csv('./gbm_predictions/gbm2.csv', header = T)


#0.02 : 3.23,0.2319727
#0.05 : 3.33,0.1836582
#0.07 : 3.346,0.1687964
#0.08 : 3.347,0.1629945
#0.09 : 3.348,0.1580418
#0.095: 3.344,0.15576
#0.1  : 3.34,0.1536327
#0.12 : 3.32,0.1460782
#0.15 : 3.29,0.13722
#0.3  : 3.2,0.1099436
#0.5  : 3.0,0.08818182
#0.705: 2.9,0.06952

percent <- c(0.2319727,0.1836582,0.1687964,0.1629945,0.1580418,0.15576,0.1536327,0.1460782,0.13722,0.1099436,0.08818182,0.06952)

score <- c(3.23,3.33,3.346,3.347,3.348,3.344,3.34,3.32,3.29,3.2,3.0,2.9)

df <- as.data.frame(cbind(percent,score))
class(df$score)
plot(df)

ggplot(data=df, aes(x=percent, y=score)) + xlab('Percentage of Signal') + ylab('AMS Score') + 
  geom_line(colour="blue", linetype="dashed", size=1.5) + 
  geom_point(colour="blue", size=4, shape=21, fill="white")


round(nrow(subset(test_15, test_15$s>0.705)))/550000


threshold = 0.08
filename = './submissions/gbm0.095.csv'

combined_output = rbind.data.frame(output1, output2, output3)
combined_output[round(nrow(combined_output) * 0.85), 2]
data1 = test_jet_0
id1 = data.frame(EventId = test_jet_0_raw$EventId)
prediction1 = rep('b', nrow(data1))
prediction1[output1[, 2] >= threshold ] = 's'


data2 = test_jet_1
id2 = data.frame(EventId = test_jet_1_raw$EventId)
prediction2 = rep('b', nrow(data2))
prediction2[output2[, 2] >= threshold ] = 's'


data3 = test_jet_2_3
id3 = data.frame(EventId = test_jet_2_3_raw$EventId)
prediction3 = rep('b', nrow(data3))
prediction3[output3[, 2] >= threshold ] = 's'


combined_output = rbind.data.frame(output1, output2, output3)
weightRank = rank(combined_output[, 2], ties.method= "random")
combined_id = rbind.data.frame(id1, id2, id3)
combined_prediction = c(prediction1, prediction2, prediction3)
submission = data.frame(EventId = combined_id, RankOrder = weightRank, Class = combined_prediction)
test_15 <- cbind(submission,combined_output)
write.csv(submission, filename, row.names = F)

sum(combined_prediction == 's') / length(combined_prediction)



#-------------------------------------------------
# Bagged Adaboost
#-------------------------------------------------
ctrl = trainControl(method = "repeatedcv",number = 5,
                    summaryFunction = AMS_summary,
                    selectionFunction = 'oneSE')

gbmGrid <-  expand.grid(mfinal = c(), 
                        maxdepth = c())
start = proc.time()
# train model
m_gbm = train(x=x, y=y, 
              method="gbm", weights=weight, 
              verbose=TRUE, trControl=ctrl, 
              tuneGrid = gbmGrid, metric="AMS")
m_gbm$bestTune
proc.time() - start



#-------------------------------------------------
# xgboost
#-------------------------------------------------
library(xgboost)

registerDoMC(cores = 36)
set.seed(80)
num = 10000
toy_data1 = train_jet_0[sample(nrow(train_jet_0), num), ]

x = toy_data1[, 1:(ncol(toy_data1)-2)]
weight = toy_data1[, ncol(toy_data1)-1]
y = toy_data1[, ncol(toy_data1)]

xgbGrid = expand.grid(nrounds = c(500,1000),
                       eta = c(0.01, 0.1),
                       max_depth = c(4,10),
                       gamma = c(1),
                       colsample_bytree = c(1),
                       min_child_weight = c(1)
                       )

ctrl = trainControl(method = "repeatedcv", number = 10,
                    summaryFunction = AMS_summary,
                    selectionFunction = 'oneSE')

start = proc.time()
m_xgb = train(x=x, y=y,
              method='xgbTree', weights = weight,
              trControl = ctrl, verbose = T,
              tuneGrid = xgbGrid, metric = "AMS")
              
proc.time() - start

m_xgb$results

#-------------------------------------------------
# Ensemble with caretStack 
#-------------------------------------------------



num = 10000
toy_data1 = train_jet_1[sample(nrow(train_jet_1), num), ]

x = toy_data1[, 1:(ncol(toy_data1)-2)]
weight = toy_data1[, ncol(toy_data1)-1]
y = toy_data1[, ncol(toy_data1)]

# create submodels
ctl <- trainControl(method="repeatedcv", number=10, repeats = 3,
                    savePredictions='final', #classProbs=TRUE,
                    summaryFunction = AMS_summary,
                    selectionFunction = 'oneSE')

# algorithmList <- c('mlpWeightDecayML', 'gbm', 'rf')
# algorithmList <- c('rf', 'AdaBag', 'avNNet')
# algorithmList <- c('rf',  'nnet', 'nbDiscrete', 'knn')
### tried 'LogitBoost' & 'rf'    -- 0.66 ('rf')
### tried 'lda', 'rf'            -- 0.72 ('rf')
### tried 'svmRadial', 'rf'      -- 0.78 ('rf')
### 

# caretList -- builds a list of models using training data
start = proc.time()
models <- caretList(x=x, y=y, weights=weight, metric = 'AMS',
                    #verbose=TRUE,
                    trControl=ctl,
                    tuneList = list(
                      # xgb1=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = c(500),
                      #                                                            eta = c(0.01, 0.1),
                      #                                                            max_depth = c(4,6),
                      #                                                            gamma = c(1),
                      #                                                            colsample_bytree = c(1),
                      #                                                            min_child_weight = c(1)),
                      #                     trace=FALSE),
                      # xgb2=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = c(1000),
                      #                                                            eta = c(0.01, 0.1),
                      #                                                            max_depth = c(4,6),
                      #                                                            gamma = c(1),
                      #                                                            colsample_bytree = c(1),
                      #                                                            min_child_weight = c(1)),
                      #                     trace=FALSE))#,
                      # rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=1)),
                      # rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2))#,
                      # xgb=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = c(500,1000),
                      #                                                           eta = c(0.01, 0.1),
                      #                                                           max_depth = c(4,6),
                      #                                                           gamma = c(0.8, 0.5),
                      #                                                           colsample_bytree = c(1),
                      #                                                           min_child_weight = c(1)),
                      #                    trace=FALSE)
                    ))
proc.time() - start
results <- resamples(models)
summary(results)
dotplot(results)


#-------------prediction
rf_pred <- predict(models$rf2, newdata=test_jet_1, type = 'prob')
rf_pred

# correlation between results
modelCor(results)
splom(results)


# correlation between results
modelCor(results)
splom(results)


# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats = 3,
                             savePredictions=TRUE, classProbs=TRUE,
                             summaryFunction = AMS_summary,
                             selectionFunction = 'oneSE')
set.seed(seed)
# caretStack -- uses a specified caret funtion to blend the predictions of the models
stack.glm <- caretStack(models, method="glm", metric="AMS", trControl=stackControl)
print(stack.glm)


# stack using random forest
set.seed(seed)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)



###############################
#####   Make Submission   #####
###############################

model_jet_0 = models$rf2
model_jet_1 = models$rf2
model_jet_2_3 = models$rf2

threshold = 0.002

makeSubmission = function(model1, data1, id1,
                          model2, data2, id2,
                          model3, data3, id3,
                          threshold = 0.002) {
  output1 = predict(model1, data1, type = 'prob')
  prediction1 = rep('b', nrow(data1))
  predicted1[output1[, 2] >= threshold ] = 's'
  
  output2 = predict(model2, data2, type = 'prob')
  prediction2 = rep('b', nrow(data2))
  predicted2[output2[, 2] >= threshold ] = 's'
  
  output3 = predict(model3, data3, type = 'prob')
  prediction3 = rep('b', nrow(data3))
  predicted3[output3[, 2] >= threshold ] = 's'
  
  combined_output = rbind.data.frame(output1, output2, output3)
  weightRank = rank(combined_output[, 2], ties.method= "random")
  combined_id = rbind.data.frame(id1, id2, id3)
  combined_prediction = rbind.data.frame(prediction1, prediction2, prediction3)
  submission = data.frame(EventId = combined_id, RankOrder = weightRank, Class = combined_prediction)
  return(submission)
}


final = makeSubmission(model_jet_0, test_jet_0, test_jet_0_raw$EventId,
                       model_jet_1, test_jet_1, test_jet_1_raw$EventId,
                       model_jet_2_3, test_jet_2_3, test_jet_2_3_raw$EventId)

write.csv(final, 'SecretPipeline1.csv', row.names = F)



