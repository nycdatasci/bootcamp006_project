##############################################
###       Kaggle Project- Higgs Boson      ###
###               Team 7-Sigma             ###
###             Amy Tzu-Yu Chen            ###
###            Chia-An Anne Chen           ###
###             Gregory Domingo            ###
###             Spencer Stebbins           ###
###              Tyler Knutson             ###
##############################################

#############################
####  Preprocessing data ####
#############################
# Load our customized metric function.
source('helper.R')

# Parallel Processing Setting
library(doMC)
registerDoMC(cores = 6)

# Read data and mark 999.0 as NAs
dfTrain = read.csv('training.csv', header=T)
dfTest = read.csv('test.csv', header=T)
dfTrain[dfTrain == -999.0] = NA
dfTest[dfTest == -999.0] = NA
testId = dfTest$EventId

str(dfTrain)

weight = dfTrain$Weight
labels = dfTrain$Label

train = dfTrain[, -c(1,32,33)]
test = dfTest[,-1]

sum(complete.cases(train)) # 68114

############################
##         PCA            ##
############################
library(psych)
scaled_train= as.data.frame(scale(train,center = T,scale = T))
str(scaled_train)
fa.parallel(scaled_train, 
            n.obs = nrow(scaled_train), 
            fa = "pc", 
            n.iter = 100) 
abline(h = 1) 
pc_train = principal(scaled_train, 
                     nfactors =10,
                     rotate = "none")
pc_train

##############################################
##         Functions for Xgboost            ##
##############################################
library(xgboost)
# subset my own training and testing
set.seed(0)
idx = sample.split(dfTrain$Label, SplitRatio = .7)
idx = which(idx == TRUE)
subTrain = dfTrain[idx,]
mytrain = subTrain[, -c(1,32,33)]
subTest = dfTrain[-idx,]
mytest = subTest[, -c(1,32,33)]

# train xgboost with own sub train
mytrain.mat = as.matrix(mytrain)
labels =  as.numeric(subTrain$Label) - 1
weight =  as.numeric(subTrain$Weight) * nrow(subTest) / length(labels)
mytrain.xgb_matrix = xgb.DMatrix(mytrain.mat, label = labels, weight = weight, missing = NA)
pos_weight = sum(weight * (labels=="1"))
neg_weight = sum(weight * (labels=="0"))
scale_pos_weight = neg_weight / pos_weight

train_xgb = function(eta, max_depth, nround){
  param = list(objective = "binary:logitraw",
               scale_pos_weight = scale_pos_weight,
               eta = eta, 
               max_depth = max_depth, 
               eval_metric = "auc",
               eval_metric = "ams@0")
  bst = xgb.train(param, mytrain.xgb_matrix, nround)
  return (bst)
}

get_prediction = function(input,sub,threshold, model){
  data = as.matrix(input)
  plabels =  as.numeric(sub$Label) - 1
  pweight =  as.numeric(sub$Weight) * nrow(subTest) / length(labels)
  pxgb_matrix = xgb.DMatrix(data, label = plabels, weight = pweight, missing = NA)
  ypred = predict(model, pxgb_matrix)
  rorder = rank(ypred, ties.method= "random")
  ntop = length(rorder) - as.integer(threshold*length(rorder))
  plabel = ifelse(rorder > ntop, "s", "b")
  return (plabel)
}

get_accuracy = function(predicted, expected){
  tb = table(predicted, expected)
  accuracy = (tb[1,1] + tb[2,2])/sum(tb)
  return (accuracy)
}

get_ams = function(sub,predicted){
  df = data.frame(obs = sub$Label, pred = predicted, weights = sub$Weight)
  return (AMS_summary(df))
}

get_submission = function(eta, depth, nrounds, threshold){
  train_data = as.matrix(train)
  labels =  as.numeric(dfTrain$Label) - 1
  weight =  as.numeric(dfTrain$Weight) * nrow(dfTest) / length(labels)
  xgb_matrix_train = xgb.DMatrix(train_data, label = labels, weight = weight, missing = NA)
  pos_weight = sum(weight * (labels=="1"))
  neg_weight = sum(weight * (labels=="0"))
  param = list(objective = "binary:logitraw",
               scale_pos_weight = neg_weight / pos_weight,
               eta = eta,
               max_depth = depth,
               eval_metric = "auc",
               eval_metric = "ams@0")
  model = xgb.train(param, xgb_matrix_train, nrounds)
  data = as.matrix(test)
  xgb_matrix = xgb.DMatrix(data, missing = NA)
  ypred = predict(model, xgb_matrix)
  rorder = rank(ypred, ties.method= "random")
  ntop = length(rorder) - as.integer(threshold*length(rorder))
  plabel = ifelse(rorder > ntop, "s", "b")
  submission = data.frame(EventId = testId, RankOrder = rorder, Class = plabel)
  return (submission)
}

################################################################
##        Xgboost with default parameters           ############
################################################################
# run xgboost with default parameters
# model = train_xgb(eta, depth, nrounds)
# get_prediction = function(input,sub,threshold, model)
# get_ams = function(sub,predicted)
set.seed(0)
default.model = train_xgb(0.3, 6, 100)
default.predicted.train = get_prediction(mytrain,subTrain,0.15, default.model)
default.accuracy.train = get_accuracy(default.predicted.train, subTrain$Label) #0.7846
default.ams_sum.train = get_ams(subTrain,default.predicted.train) #3.798783

default.predicted.test = get_prediction(mytest,subTest,0.15, default.model)
default.accuracy.test = get_accuracy(default.predicted.test, subTest$Label) #0.78
default.ams_sum.test = get_ams(subTest,default.predicted.test)  #2.033193 
#test ams is a lot lower than train ams, overfitting

data = as.matrix(test)
xgb_matrix = xgb.DMatrix(data, missing = NA)
ypred = predict(default.model, xgb_matrix)
rorder = rank(ypred, ties.method= "random")
threshold = 0.15
ntop = length(rorder) - as.integer(threshold*length(rorder))
plabel = ifelse(rorder > ntop, "s", "b")
submission = data.frame(EventId = testId, RankOrder = rorder, Class = plabel)
write.csv(submission, "default_xgboost_submission.csv", row.names = FALSE)
# rank 738, score 3.52866	

#################################################################
#####   Xgboost Cross Validation using Subtest Accuracy/AMS #####
#################################################################
# model = train_xgb(eta, depth, nrounds)
# get_prediction = function(input,sub,threshold, model)
# get_ams = function(sub,predicted)
result_df = data.frame(eta = NULL, max_depth= NULL,  nrounds = NULL, threshold = NULL, accuracy = NULL, ams_sum = NULL)
counter = 1 
# record the elapsed time  
ptm <- proc.time()
for (eta in seq(0.02, 0.4, 0.08)){
  for (depth in seq(5, 10, 1)){
    for (nrounds in seq(25, 90, 10)){
      for (threshold in seq(0.1, 0.3, 0.05)){
        print (counter)
        model = train_xgb(eta, depth, nrounds)
        predicted = get_prediction(mytest,subTest,threshold, model)
        accuracy = get_accuracy(predicted, subTest$Label)
        ams_sum = get_ams(subTest,predicted)
        vec = c(eta, depth, nrounds, threshold, accuracy, ams_sum)
        result_df = rbind(result_df, vec) 
        counter = counter +1
      }   
    }   
  }  
}

proc.time()-ptm
names(result_df) = c("eta", "max_depth", "nrounds", "threshold", "accuracy", "ams_sum")
write.csv(result_df, "xgb_cross_validation.csv", row.names = FALSE)  #save cv result into a csv

which(result_df$accuracy == max(result_df$accuracy)) #420th model
which(result_df$ams_sum == max(result_df$ams_sum))   #382nd model

### optimized model based on best AMS
result_df[382,]
# eta max_depth nrounds threshold  accuracy  ams_sum
# 0.1         9      85      0.15 0.7822133 2.061053
model.cv = train_xgb(0.1, 9, 85)
data = as.matrix(test)
xgb_matrix = xgb.DMatrix(data, missing = NA)
ypred = predict(model.cv, xgb_matrix)
rorder = rank(ypred, ties.method= "random")
threshold = 0.15
ntop = length(rorder) - as.integer(threshold*length(rorder))
plabel = ifelse(rorder > ntop, "s", "b")
submission = data.frame(EventId = testId, RankOrder = rorder, Class = plabel)
write.csv(submission, "cv_xgboost_submission.csv", row.names = FALSE)
# rank 515, score 3.62859

## get AMS of training 
train_data = as.matrix(train)
labels =  as.numeric(dfTrain$Label) - 1
weight =  as.numeric(dfTrain$Weight) * nrow(dfTest) / length(labels)
xgb_matrix_train = xgb.DMatrix(train_data, label = labels, weight = weight, missing = NA)
pos_weight = sum(weight * (labels=="1"))
neg_weight = sum(weight * (labels=="0"))
param = list(objective = "binary:logitraw",
             scale_pos_weight = neg_weight / pos_weight,
             eta = 0.1,
             max_depth = 9,
             eval_metric = "auc",
             eval_metric = "ams@0")
model = xgb.train(param, xgb_matrix_train, 85)
pred.cv = predict(model, xgb_matrix_train)
rorder = rank(pred.cv, ties.method= "random")
ntop = length(rorder) - as.integer(0.15*length(rorder))
plabel.cv = ifelse(rorder > ntop, "s", "b")
ams.cv = get_ams(dfTrain, plabel.cv)
ams.cv

## try more models
submission.cv = get_submission(0.1, 9, 85, 0.15)
write.csv(submission.cv, "cv_xgboost_submission.csv", row.names = FALSE)
# rank 477, score 3.64000

###### a bit over fit, so we tried decreasing number of rounds
submission.less.round = get_submission(0.1, 9, 75, 0.15)
write.csv(submission.less.round, "less_round_xgboost_submission.csv", row.names = FALSE)
# rank 505, score 3.63122

# underfit, increase depth of trees
submission.depth10 = get_submission(0.1, 10, 75, 0.15)
write.csv(submission.depth10, "depth10_xgboost_submission.csv", row.names = FALSE)
# rank 99, score 3.69895

# try adjusting eta to get better result
submission.d10.eta12 = get_submission(0.12, 10, 75, 0.15)
write.csv(submission.d10.eta12, "d10_eta12_xgboost_submission.csv", row.names = FALSE)
# rank 476, score 3.64034

####3 similar performance as model.cv, try same eta with nrounds=85
submission.eta12.85round = get_submission(0.12, 10, 85, 0.15)
write.csv(submission.eta12.85round, "round85_eta12_xgboost_submission.csv", row.names = FALSE)
# rank 509, score 3.62985

## appendix graphs
library(ggplot2)
mytrain.mat = as.matrix(train)
labels =  as.numeric(dfTrain$Label) - 1
weight =  as.numeric(dfTrain$Weight) * nrow(dfTest) / length(labels)
mytrain.xgb_matrix = xgb.DMatrix(mytrain.mat, label = labels, weight = weight, missing = NA)
pos_weight = sum(weight * (labels=="1"))
neg_weight = sum(weight * (labels=="0"))
scale_pos_weight = neg_weight / pos_weight

model.best = train_xgb(0.1, 10, 75)
xgb_imp = xgb.importance(model = model.best)
vec = as.numeric(xgb_imp$Feature) + 1
xgb_imp$Feature = names(train)[vec]
t = theme_bw() + theme(axis.title = element_text(size = 20), 
                       axis.text = element_text(size = 18),
                       legend.text = element_text(size = 20),
                       legend.title = element_text(size = 20),
                       plot.title = element_text(size = 25))

g = ggplot(xgb_imp, aes(x= reorder(Feature, Frequence), y = Frequence)) + 
  geom_bar(stat="identity") + coord_flip()
plot_weight = g + t + labs(title = "Weight of Each Variable", y = "Weight", x = "")
plot_weight

g = ggplot(xgb_imp, aes(x= reorder(Feature, Cover), y = Cover)) + 
  geom_bar(stat="identity") + coord_flip()
plot_cover = g + t + labs(title = "Cover of Each Variable", y = "Cover", x = "")
plot_cover

g = ggplot(xgb_imp, aes(x= reorder(Feature, Gain), y = Gain)) + 
  geom_bar(stat="identity") + coord_flip()
plot_gain = g + t + labs(title = "Gain of Each Variable", y = "Gain", x = "")
plot_gain