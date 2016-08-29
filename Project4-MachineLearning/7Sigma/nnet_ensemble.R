###  Main Function ###
######################
library(caret)
library(doMC)
library(pROC)
library(xgboost)
library(methods)
library(mlbench)
library(caretEnsemble)
library(caTools)
library(neuralnet)
library(readr)
#############################
####  Preprocessing data ####
#############################
# set directory
# setwd("~/Documents/NYCDSA/Projects/Kaggle")
# Load our customized metric function.
source('./helper.R')

# Read data and mark 999.0 as NAs
dfTrain = read_csv('./training.csv')
dfTest = read_csv('./test.csv')
# dfTrain = dfTrain[1:1000,]
# dfTrain[dfTrain == -999.0] = NA
# dfTest[dfTest == -999.0] = NA
testId = dfTest$EventId

weight = dfTrain$Weight
labels = dfTrain$Label


# subset my own training and testing
set.seed(0)
idx = sample.split(dfTrain$Label, SplitRatio = .7)
idx = which(idx == TRUE)
subTrain = dfTrain[idx,]
subTest = dfTrain[-idx,]
mytrain = subTrain[, -c(1,32,33)]
mytest = subTest[, -c(1,32,33)]

train_neural = function(data, hidden_nodes){
    for(i in 1:ncol(data)){
        data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
    }
    return(train(Label ~ ., 
                 method="nnet", 
                 data=data, 
                 tuneGrid = expand.grid(
                     size=hidden_nodes, 
                     decay=0.1
                 )
    ))
}

get_prediction = function(data, threshold, model, type='nnet'){
    #labels =  as.numeric(data$Label) - 1
    #weight =  as.numeric(data$Weight) * nrow(data) / length(labels)
    if (type == 'xgb') {
        data = as.matrix(data)
        target = xgb.DMatrix(data, label = labels, weight = weight, missing = NA)
    } else {
        # impute mean on NA
        for(i in 1:(ncol(data)-1)){
            #colname = colnames(data)[i]
            mm = mean(data[,i], na.rm = TRUE)
            data[is.na(data[,i]), i] <- mm #mean(as.numeric(data[,i]), na.rm = TRUE)
        }
        target = data
    }
    ypred = predict(model, target, type='prob')
    rorder = rank(ypred[,2], ties.method= "random")
    ntop = length(rorder) - as.integer(threshold*length(rorder))
    plabel = ifelse(rorder > ntop, "s", "b")
    return (plabel)
}


get_ams = function(data,predicted){
    df = data.frame(obs = data$Label, pred = predicted, weights = data$Weight)
    #df = data.frame(obs = label, pred = predicted, weights = weight)
    return (AMS_summary(df))
}

cv_nnet_models = function(data, validation,  start, end) {
    print(paste('Cross validating neural network on ams score with nodes from',start,'to',end))
    neural_df = data.frame(hidden_nodes= NULL, threshold=NULL, ams_sum = NULL)
    for (hidden_nodes in seq(start,end,1)){
        print(paste('Training with', hidden_nodes,'hidden nodes'))
        model = train_neural(data, hidden_nodes)
        bst_ams= 0
        bst_model = NULL
        for (i in seq(0,1,.025)) {
            print(paste('Predicting layer with',hidden_nodes,'nodes & threshold: ', i))
            predicted = get_prediction(validation, i, model, 'nnet')
            # accuracy = get_accuracy(predicted, subTrain$Label)
            ams_sum = get_ams(validation, predicted)
            if (ams_sum > bst_ams) {
                vec = c(hidden_nodes, i, ams_sum)
                print(vec)
                bst_ams = ams_sum
                bst_model = vec
            }
        }
        print(paste('Choosing best model with ams score:', bst_ams))
        neural_df = rbind(neural_df, bst_model) 
    }
    return(neural_df)
}

results<- cv_nnet_models(subTrain, subTest, 5,5)
# pick best model
model <- train_neural(dfTrain, 4)
# nnet results
names(results) = c("hidden_nodes", "threshold", "ams")
write.csv(results, "nnet_cross_validation30.csv", row.names = FALSE)
results[which(results$ams_sum == max(results$ams_sum)),]


# #use best xgbBoost and nNet models based on ams
control <- trainControl(method="repeatedcv", number=2, savePredictions='final', classProbs=TRUE, summaryFunction=AMS_summary)
model_list <- caretList(
    labels~.,
    data=dfTrain,
    trControl= control,
    metric="AMS",
    tuneList=list(
        # NEEDS WORK
        xgb=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(
            eta=0.1, max_depth=10, nrounds=75
        )),
        nn=caretModelSpec(method="nnet", trace=FALSE,tuneGrid=expand.grid(
            size=4, decay=0.1
        ))
    )
)
# 
# results <- resamples(models)
# # correlation between model results
# modelCor(results)
# splom(results)
# 
# greedy_ensemble <- caretEnsemble(
#     model_list, 
#     metric="AMS",
#     trControl=trainControl(
#         number=2,
#         summaryFunction=AMS_summary,
#         savePredictions='final',
#         classProbs=TRUE
#     ))
# summary(greedy_ensemble)
# 
# # perform ensemble model on test data
# model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
# model_preds <- lapply(model_preds, function(x) x[,"M"])
# model_preds <- data.frame(model_preds)
# ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
# model_preds$ensemble <- ens_preds
# caTools::colAUC(model_preds, testing$Class)
