setwd("~/Higgs Boson Kaggle Challenge")
#Load Libraries and Helper Files
library(caret)
library(xgboost)
library(pROC)
library(plyr)
library(VIM)
source('helper.R')

#Load data and prepare for use
dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)
dfTrain.jet0 <- read.csv('dfTrain.jet0.csv', header=T)
dfTest.jet0 <- read.csv('dfTest.jet0.csv', header=T)
dfTrain.jet1 <- read.csv('dfTrain.jet1.csv', header=T)
dfTest.jet1 <- read.csv('dfTest.jet1.csv', header=T)
dfTrain.jet2 <- read.csv('dfTrain.jet2.csv', header=T)
dfTest.jet2 <- read.csv('dfTest.jet2.csv', header=T)

############################################################################################
##########################################Select What Datasets to use#######################
############################################################################################

source='full' #To run all data as one set
iterations=c(1)
#tuned_paramaters=data.frame(eta=c(.05),max_depth=c(12))
xgb_grid=expand.grid(eta=c(.05,.075,.10), max_depth=c(6,7,8,9,10),nround=c(120,200,300))

#source = 'split'
#iterations = c(1,2,3) # To run individual datasets
#tuned_params=data.frame(eta=c(0.25,0.2,0.2),max_depth=c(12,10,12))

############################################################################################
############################TUNE PARAMETERS ROUND 1#########################################
############################################################################################
result_table=NULL
set.seed(1618)
for (i in iterations) {
  
#############################Filter Full Data Set or Individual Samples for testing#########

  if (source=='full'){
    dtrain=dfTrain
    dtest = dfTest
  } else {
    if (i==1){
      dtrain = dfTrain.jet2
      dtest = dfTest.jet2
    } else if (i==2) {
      dtrain = dfTrain.jet1
      dtest = dfTest.jet1
    } else if (i==3) {
      dtrain = dfTrain.jet0
      dtest = dfTest.jet0
    }
  }
  #######Generate 50% training set for tuning
  train_set=sample(1:nrow(dtrain), 5*nrow(dtrain)/10)
  dtrain.test=dtrain[-train_set,]
  dtrain=dtrain[train_set,]

  #######Prepare dataset to test on
  train.testsize=nrow(dtrain.test)  
  dtrain.test$Label <- dtrain.test$Label == "s"
  dtrain.testlabel <- as.numeric(dtrain.test$Label)
  dtrain.testdata <- as.matrix(dtrain.test[,-which(names(dtrain.test)%in%c('EventId','Weight','Label'))])
  dtrain.testweight <- as.numeric(dtrain.test$Weight) * train.testsize / length(dtrain.testlabel)
  
  #######Prepare data for training
  trainsize=nrow(dtrain)  
  dtrain$Label <- dtrain$Label == "s"
  label <- as.numeric(dtrain$Label)
  data <- as.matrix(dtrain[,-which(names(dtrain)%in%c('EventId','Weight','Label'))])
  weight <- as.numeric(dtrain$Weight) * trainsize / length(label)
  
  sumwpos <- sum(weight * (label==1.0))
  sumwneg <- sum(weight * (label==0.0))
  
  xgmat <- xgb.DMatrix(data, label = label, weight = weight, missing = -999.0)
  
  for (j in c(1:nrow(xgb_grid))) {
    param <- list("objective" = "binary:logistic",
                  "scale_pos_weight" = sumwneg / sumwpos,
                  "bst:eta" = xgb_grid$eta[j],
                  "bst:max_depth" = xgb_grid$max_depth[j],
                  "eval_metric" = "auc",
                  "eval_metric" = "ams@0.15",
                  "silent" = 1,
                  "nthread" = 16)
    #watchlist <- list("train" = xgmat)
    nround = xgb_grid$nround[j]
    print(paste("loading data end, start to boost trees",((j*(i-1)) + j)))
    bst = xgb.train(param, xgmat,nround) #, watchlist )
    train.test.pred = predict(bst,dtrain.testdata,missing=-999)
    
    test_mat = seq(1,99)
    AMS_result_table=NULL
    for (k in test_mat) {
      threshold=test_mat[k]*.01
      pred = ifelse(train.test.pred>=threshold,'s','b')
      ams_label = ifelse(dtrain.testlabel==1,'s','b')
      AMS_result_table[k]=AMS(ams_label,pred,dtrain.testweight)
    }
    threshold = which(AMS_result_table==max(AMS_result_table))*.01
    
    if (i==1){
      result_table$jet2thresh[j] = threshold
      result_table$jet2AMS[j] = max(AMS_result_table)
    } else if (i==2) {
      result_table$jet1thresh[j] = threshold
      result_table$jet1AMS[j] = max(AMS_result_table)
    } else {
      result_table$jet0thresh[j] = threshold
      result_table$jet0AMS[j] = max(AMS_result_table)
    }
  }
}

result_table

results = cbind(result_table,xgb_grid)
write.csv(results,'grid_search_results.csv',row.names=FALSE)
# plot(results$jet2AMS,results$eta)
# plot(results$jet2AMS,results$max_depth)
# plot(results$jet2AMS,results$nround)


############################################################################################################
###########Take best tuning parameters from this test and generate a new model on full training data.#######
############################################################################################################

set.seed(1618)

source='full' #To run all data as one set
iterations=c(1)
tuned_params=data.frame(eta=c(.05),max_depth=c(12),nrounds=c(120))


#source = 'split'
#iterations = c(1,2,3)
#tuned_params=data.frame(eta=c(0.25,0.2,0.2),max_depth=c(12,10,12))

############COMPLETE CASES####################################################
train_ensemble = data.frame(EventId = NULL, RankOrder = NULL, Prob = NULL, Class = NULL)
test_ensemble = data.frame(EventId = NULL, RankOrder = NULL, Prob = NULL, Class = NULL)

for (i in iterations) {
  if (source=='full'){
    dtrain=dfTrain
    dtest = dfTest
  } else {
    if (i==1){
      dtrain = dfTrain.jet2
      dtest = dfTest.jet2
    } else if (i==2) {
      dtrain = dfTrain.jet1
      dtest = dfTest.jet1
    } else if (i==3) {
      dtrain = dfTrain.jet0
      dtest = dfTest.jet0
    }
  }
  testsize <- nrow(dtrain)
  dtrain$Label <- dtrain$Label == "s"
  label <- as.numeric(dtrain$Label)
  data <- as.matrix(dtrain[,-which(names(dtrain)%in%c('EventId','Weight','Label'))])
  weight <- as.numeric(dtrain$Weight) * testsize / length(label)
  
  sumwpos <- sum(weight * (label==1.0))
  sumwneg <- sum(weight * (label==0.0))

  xgmat <- xgb.DMatrix(data, label = label, weight = weight, missing = -999.0)
  param <- list("objective" = "binary:logistic",
                "scale_pos_weight" = sumwneg / sumwpos,
                "bst:eta" = tuned_params$eta[i],
                "bst:max_depth" = tuned_params$max_depth[i],
                "eval_metric" = "auc",
                "eval_metric" = "ams@0.15",
                "silent" = 1,
                "nthread" = 16,
                "subsample"=0.8)
  watchlist <- list("train" = xgmat)
  nround = tuned_params$nrounds[i]
  print ("loading data end, start to boost trees")
  bst = xgb.train(param, xgmat, nround, watchlist );
  print ('finish training')
  
###########Find optimal threshold on AMS scoring
  bst.pred=predict(bst,data)
  
  test_mat = seq(1,999)
  AMS_result_table=NULL
  
  for (j in test_mat) {
    threshold=test_mat[j]*.001
    pred = ifelse(bst.pred>=threshold,'s','b')
    ams_label = ifelse(label==1,'s','b')
    AMS_result_table[j]=AMS(ams_label,pred,weight)
  }
  
  threshold = which(AMS_result_table==max(AMS_result_table))*.001
  # threshold=0.95
  
  ######################Produce Enesmble Output on Training Data ###########################
  idx = dtrain[[1]] # Use to save to probabilities file for ensembling
  
  xgmat <- xgb.DMatrix(data, missing = -999.0)
  ypred <- predict(bst, xgmat)
  
  rorder <- rank(ypred, ties.method="first")
  plabel <- ifelse(ypred>=threshold, "s", "b")
  outdata <- list("EventId" = idx,
                  "RankOrder" = rorder,
                  "Prob" = ypred,
                  "Class" = plabel)
  
  train_ensemble = rbind(train_ensemble,as.data.frame(outdata))
  
  #########################Produce Ensemble Output on Test Data #############################
  data <- as.matrix(dtest[,-which(names(dtrain)%in%c('EventId','Weight','Label'))])
  
  idx = dtest[[1]] # Use to save submittable output
   
  xgmat <- xgb.DMatrix(data, missing = -999.0)
  ypred <- predict(bst, xgmat)
  
  rorder <- rank(ypred, ties.method="first")
  plabel <- ifelse(ypred>=threshold, "s", "b")
  outdata <- list("EventId" = idx,
                  "RankOrder" = rorder,
                  "Prob" = ypred,
                  "Class" = plabel)
  
  test_ensemble = rbind(test_ensemble,as.data.frame(outdata))
  
}

test_ensemble$RankOrder = rank(test_ensemble$RankOrder, ties='random')
train_ensemble$RankOrder = rank(train_ensemble$RankOrder, ties='random')

xgb.importance(model=bst)

#Check to see how many results
nrow(test_ensemble[test_ensemble$Prob>=threshold,])
nrow(test_ensemble[test_ensemble$Class=='s',])

write.csv(test_ensemble, "xgb_ensemble_output_test.csv", row.names=FALSE)
write.csv(test_ensemble[,-c(3)], "xgb_submission_test.csv", row.names=FALSE)

write.csv(train_ensemble, "xgb_ensemble_output_train.csv", row.names=FALSE)
write.csv(train_ensemble[,-c(3)], "xgb_submission_train.csv", row.names=FALSE)

