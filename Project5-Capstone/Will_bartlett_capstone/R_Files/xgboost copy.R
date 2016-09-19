#XGBOOST
setwd('~/Desktop/Data_Science/GCS_Folder/Rdata_Files')
load('ts_data.RData')
load('ts_stats.RData')
load('model_data_min.RData')
library(dplyr)
library(gridExtra)
library(tree)
library(MASS)
library(ggplot2)
library(gbm)
library(randomForest)
library(xgboost)

#make label binary
model_data_min$last_score = ifelse(model_data_min$last_score>11, 1, 0)
model_data_min = model_data_min[,-c(1,13)]


train_ind = sample(1:nrow(model_data_min), .7*nrow(model_data_min))
train_df = model_data_min[train_ind,]
test_df = model_data_min[-train_ind,]
test_ind = seq(1,nrow(model_data_min), 1)[-train_ind]

train_data = as.matrix(train_df[,-c(3)])
train_label = as.matrix(train_df[,3])
train_list = list(data = train_data, label = train_label)

test_data = as.matrix(test_df[,-c(3)])
test_label = as.matrix(test_df[,3])
test_list = list(data = test_data, label = test_label)

xgb = xgboost(data = train_list$data, 
              label = train_list$label, 
              nrounds = 200,
              objective = "binary:logistic")

predictions = predict(xgb, test_list$data)

table = table(round(predictions), test_label)
sum(table[1],table[4])/sum(table)










