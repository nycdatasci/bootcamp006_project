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
dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)
dfTrain.jet0 <- read.csv('dfTrain.jet0.csv', header=T)
dfTest.jet0 <- read.csv('dfTest.jet0.csv', header=T)
dfTrain.jet1 <- read.csv('dfTrain.jet1.csv', header=T)
dfTest.jet1 <- read.csv('dfTest.jet1.csv', header=T)
dfTrain.jet2 <- read.csv('dfTrain.jet2.csv', header=T)
dfTest.jet2 <- read.csv('dfTest.jet2.csv', header=T)


#######################################################################################
##################################JET 0 DATA###########################################
#######################################################################################

train_ensemble_split = data.frame(EventId = NULL, RankOrder = NULL, Prob = NULL, Class = NULL)
test_ensemble_split = data.frame(EventId = NULL, RankOrder = NULL, Prob = NULL, Class = NULL)

testsize <- 550000
dtrain=dfTrain.jet0
dtest = dfTest.jet0
dtrain$Label <- dtrain$Label == "s"
label=dtrain$Label
weight=dtrain$Weight
data <- dtrain[,-which(names(dtrain)%in%c('EventId','Weight'))]

higgs.logit = glm(Label~.,data=data,family='binomial')

summary(higgs.logit)

higgs.logit.reduced = glm(Label~ DER_mass_MMC +DER_mass_transverse_met_lep +DER_mass_vis +DER_deltar_tau_lep
                          +DER_pt_ratio_lep_tau +PRI_met_phi +PRI_met_sumet,
                          data = data,
                          family='binomial')

summary(higgs.logit.reduced)

test_mat = seq(1,99)
AMS_result_table=NULL

for (j in test_mat) {
  threshold=test_mat[j]*.01
  higgs.logit.prob=exp(higgs.logit.reduced$fitted.values)/(1 + exp(higgs.logit.reduced$fitted.values))
  pred = ifelse(higgs.logit.prob>=threshold,'s','b')
  ams_label = ifelse(label==1,'s','b')
  AMS_result_table[j]=AMS(ams_label,pred,weight)
}

threshold = which(AMS_result_table==max(AMS_result_table))*.01
max(AMS_result_table)

#############################Build Train Data Ensembling Output################
idx = dtrain[[1]]

ypred <- predict(higgs.logit.reduced, data)

rorder <- rank(ypred, ties.method="first")
prob=exp(ypred)/(1 + exp(ypred))
plabel <- ifelse(prob >= threshold, "s", "b")
outdata <- list("EventId" = idx,
                "RankOrder" = rorder,
                "Prob" = prob,
                "Class" = plabel)

train_ensemble_split = rbind(train_ensemble_split,as.data.frame(outdata))

############################Build Test Data Ensembling Output##################
data <- dtest[,-which(names(dtest)%in%c('EventId','Weight','Label'))]

ypred <- predict(higgs.logit.reduced, data)

rorder <- rank(ypred, ties.method="first")
idx <- dtest[[1]]
prob=exp(ypred)/(1 + exp(ypred))
plabel <- ifelse(prob >= threshold, "s", "b")
outdata <- list("EventId" = idx,
                "RankOrder" = rorder,
                "Prob" = prob,
                "Class" = plabel)

test_ensemble_split = rbind(test_ensemble_split,as.data.frame(outdata))



#######################################################################################
##################################JET 1 DATA###########################################
#######################################################################################
dtrain=dfTrain.jet1
dtest = dfTest.jet1
dtrain$Label <- dtrain$Label == "s"
label=dtrain$Label
weight=dtrain$Weight
data <- dtrain[,-which(names(dtrain)%in%c('EventId','Weight'))]

higgs.logit = glm(Label~.,data=data,family='binomial')

summary(higgs.logit)

higgs.logit.reduced = glm(Label~ DER_mass_MMC +DER_mass_transverse_met_lep +DER_mass_vis +DER_deltar_tau_lep
                          +DER_pt_tot +DER_pt_ratio_lep_tau +DER_met_phi_centrality +PRI_met +PRI_met_sumet +PRI_lep_eta,
                          data = data,
                          family='binomial')

summary(higgs.logit.reduced)

test_mat = seq(1,99)
AMS_result_table=NULL

for (j in test_mat) {
  threshold=test_mat[j]*.01
  higgs.logit.prob=exp(higgs.logit$fitted.values)/(1 + exp(higgs.logit$fitted.values))
  pred = ifelse(higgs.logit.prob>=threshold,'s','b')
  ams_label = ifelse(label==1,'s','b')
  AMS_result_table[j]=AMS(ams_label,pred,weight)
}

threshold = which(AMS_result_table==max(AMS_result_table))*.01
max(AMS_result_table)

#############################Build Train Data Ensembling Output################
idx = dtrain[[1]]

ypred <- predict(higgs.logit.reduced, data)
rorder <- rank(ypred, ties.method="first")
prob=exp(ypred)/(1 + exp(ypred))
plabel <- ifelse(prob >= threshold, "s", "b")
outdata <- list("EventId" = idx,
                "RankOrder" = rorder,
                "Prob" = prob,
                "Class" = plabel)

train_ensemble_split = rbind(train_ensemble_split,as.data.frame(outdata))

############################Build Test Data Ensembling Output##################
data <- dtest[,-which(names(dtest)%in%c('EventId','Weight','Label'))]

ypred <- predict(higgs.logit.reduced, data)

rorder <- rank(ypred, ties.method="first")
idx <- dtest[[1]]
# idx = dtrain[[1]]
prob=exp(ypred)/(1 + exp(ypred))
plabel <- ifelse(prob >= threshold, "s", "b")
outdata <- list("EventId" = idx,
                "RankOrder" = rorder,
                "Prob" = prob,
                "Class" = plabel)

test_ensemble_split = rbind(test_ensemble_split,as.data.frame(outdata))

#######################################################################################
##################################JET 2 DATA###########################################
#######################################################################################



dtrain=dfTrain.jet2
dtest = dfTest.jet2
dtrain$Label <- dtrain$Label == "s"
label=dtrain$Label
weight=dtrain$Weight
data <- dtrain[,-which(names(dtrain)%in%c('EventId','Weight'))]

higgs.logit = glm(Label~.,data=data,family='binomial')

summary(higgs.logit)

higgs.logit.reduced = glm(Label~. -PRI_jet_all_pt -PRI_jet_subleading_phi -PRI_jet_subleading_eta -PRI_jet_leading_phi -PRI_jet_leading_eta -PRI_lep_phi 
                          -PRI_met_phi -PRI_lep_eta -PRI_lep_pt -PRI_tau_phi -PRI_tau_eta -PRI_tau_pt -DER_sum_pt,
                          data = data,
                          family='binomial')

summary(higgs.logit.reduced)

test_mat = seq(1,99)
AMS_result_table=NULL

for (j in test_mat) {
  threshold=test_mat[j]*.01
  higgs.logit.prob=exp(higgs.logit$fitted.values)/(1 + exp(higgs.logit$fitted.values))
  pred = ifelse(higgs.logit.prob>=threshold,'s','b')
  ams_label = ifelse(label==1,'s','b')
  AMS_result_table[j]=AMS(ams_label,pred,weight)
}

threshold = which(AMS_result_table==max(AMS_result_table))*.01
max(AMS_result_table)

#############################Build Train Data Ensembling Output################
idx = dtrain[[1]]

ypred <- predict(higgs.logit.reduced, data)
rorder <- rank(ypred, ties.method="first")
prob=exp(ypred)/(1 + exp(ypred))
plabel <- ifelse(prob >= threshold, "s", "b")
outdata <- list("EventId" = idx,
                "RankOrder" = rorder,
                "Prob" = prob,
                "Class" = plabel)

train_ensemble_split = rbind(train_ensemble_split,as.data.frame(outdata))

############################Build Test Data Ensembling Output##################
data <- dtest[,-which(names(dtest)%in%c('EventId','Weight','Label'))]

ypred <- predict(higgs.logit.reduced, data)

rorder <- rank(ypred, ties.method="first")
idx <- dtest[[1]]
# idx = dtrain[[1]]
prob=exp(ypred)/(1 + exp(ypred))
plabel <- ifelse(prob >= threshold, "s", "b")
outdata <- list("EventId" = idx,
                "RankOrder" = rorder,
                "Prob" = prob,
                "Class" = plabel)

test_ensemble_split = rbind(test_ensemble_split,as.data.frame(outdata))


test_ensemble_split$RankOrder = rank(test_ensemble_split$RankOrder, ties='random')
train_ensemble_split$RankOrder = rank(train_ensemble_split$RankOrder, ties='random')


nrow(test_ensemble_split[test_ensemble_split$Prob>=threshold,])
nrow(test_ensemble_split[test_ensemble_split$Class=='s',])


write.csv(test_ensemble_split, "logit_ensemble_split_output_test.csv", row.names=FALSE)
write.csv(test_ensemble_split[,-c(3)], "logit_submission_split_test.csv", row.names=FALSE)
write.csv(train_ensemble_split, "logit_ensemble_output_split_train.csv", row.names=FALSE)
write.csv(train_ensemble_split[,-c(3)], "logit_submission_split_train.csv", row.names=FALSE)



###################################AMS on Full Data

label=dfTrain$Label=="s"
weight=dfTrain$Weight

for (j in test_mat) {
  threshold=test_mat[j]*.01
  prob=train_ensemble_split$Prob
  pred = ifelse(prob>=threshold,'s','b')
  ams_label = ifelse(label==1,'s','b')
  AMS_result_table[j]=AMS(ams_label,pred,weight)
}

max(AMS_result_table)
