setwd('C:/Users/Charles/OneDrive/higgs_kaggle')
Higgs.train = read.csv('./data/training.csv', header = T)
Higgs.test = read.csv('./data/test.csv', header = T)
Imp.train = read.csv('./data/Imp_Charles_train.csv', header = T)
Imp.test = read.csv('./data/Imp_Charles_test.csv', header = T)



head(Higgs.train)
summary(Higgs.train)

library(mice)
Higgs.train[Higgs.train == -999] = NA
Higgs.test[Higgs.test == -999] = NA
#Imputation analysis

library(dplyr)

jet23.test = Higgs.test %>% filter(PRI_jet_num > 1)
jet23.train = Higgs.train %>% filter(PRI_jet_num > 1)
md.pattern(jet23.train)

#Assume impute DER_mass_MMC by random
library(Hmisc)
jet23.train$PRI_jet_num = as.factor(jet23.train$PRI_jet_num)
jet23.train$DER_mass_MMC = Imp.train$DER_mass_MMC
jet23.test$DER_mass_MMC = Imp.test$DER_mass_MMC
#Set positive to 's'
levels(jet23.train$Label) = rev(levels(jet23.train$Label))
# trim for extreme collinearity problems, select only numerical values
#trim.matrix(cov(jet23.train %>% select(-Label, -PRI_jet_num)))
#PRI_lep_pt discarded

#For use in modeling:
library(caret)
set.seed(0)
source('Jumpstart_helper.R')

##########################
## GLM, Binomial #########
##########################
# 
# ctrl = trainControl(summaryFunction = twoClassSummary, #AMS_Summary before
#                     method = 'LGOCV',
#                     savePredictions = T,
#                     classProbs = T)
# 
# jet23.glm <- train(Label ~ . - Weight,
#                   data = jet23.train,
#                   #weights = Weight,
#                   method = 'glm',
#                   family = 'binomial',
#                   metric = 'ROC', #AMS before
#                   trControl = ctrl)
# 
# library(pROC)
# confusionMatrix(data = jet23.glm$pred$pred, reference = jet23.glm$pred$obs)

##########################
## GLMnet, Elastic Net ###
##########################

# glmnGrid = expand.grid(.alpha=1, .lambda= 10^seq(-4, -2, length = 10))
# 
# 
# ctrl = trainControl(summaryFunction = twoClassSummary, #AMS_Summary before
#                     method = 'repeatedcv',
#                     classProbs = T,
#                     repeats = 5,
#                     number = 10)
# 
# jet23.glmnet <- train(Label ~ . - Weight,
#                    data = jet23.train,
#                    preProcess = c('center','scale'),
#                    #weights = Weight,
#                    method = 'glmnet',
#                    tuneGrid = glmnGrid,
#                    family = 'binomial',
#                    metric = 'ROC', #AMS before
#                    trControl = ctrl)
# 
# library(pROC)
# confusionMatrix(data = jet23.glmnet$pred$pred, reference = jet23.glmnet$pred$obs)


#Predict the test set
#predict(jet23.cv, newdata = jet23.test, type="prob"), not sure when in stage to predict test set.




######################################
## GBM, Boosted Log (Bernoulli) ######
######################################
 
 ctrl = trainControl(summaryFunction = AMS_summary, #TwoClassSummary before
                     method = 'LGOCV',
                     savePredictions = T,
                     classProbs = T)

gbmGrid = expand.grid(interaction.depth = c(1,3,5), n.trees = 1000,
                        shrinkage = .001,
                        n.minobsinnode = c(100, 500, 2000))

jet23.gbm = train(Label ~ . - Weight,
                  data = jet23.train,  
                  tuneGrid = gbmGrid,
                  method ="gbm",
                  weights = Weight,
                  verbose = TRUE,
                  trControl = ctrl,
                  metric = "AMS")

jet23.gbm.pred = predict(jet23.gbm, newdata = jet23.train, type = "prob")

#PREDICTIONS on Train to find Cutoff Value: 0.004

library(pROC)
labels = jet23.train$Label
labels = ifelse(labels=='s', 1, 0)
auc = roc(labels, jet23.gbm.pred[,2])
plot(auc, print.thres=TRUE)

threshold = 0.004

#PREDICTIONS on Test to find probabilities, then apply cutoff Value for submission
load('Chales_imputed.Rdata')

jet23.test$DER_mass_MMC = Chales_test_remove_imputed$DER_mass_MMC
jet23.test$Weight = rep(10,160128)
jet23.test$PRI_jet_num = as.factor(jet23.test$PRI_jet_num)

jet23.gbm.pred = predict(jet23.gbm, newdata = jet23.train, type = "prob")
jet23.gbm.pred2 = ifelse(jet23.gbm.pred[,2] >= threshold, 's', 'b')
weightRank = rank(jet23.gbm.pred[,2], ties.method= "random")

jet23probs = data.frame(Probability = jet23.gbm.pred, Predicted = jet23.gbm.pred2)
save(jet23probs, file = "jet23.gbm_probs.Rdata")

##########################
## Random Forest ########
# ##########################

ctrl = trainControl(summaryFunction = AMS_summary, #AMS_Summary before
                    method = 'LGOCV',
                    verboseIter = T,
                    savePredictions = T,
                    classProbs = T)

mtryValues = c(4, 6, 8, 10)

jet23v2 = jet23.train %>% select(-Weight)
jet23.weight = jet23.train$Weight

jet23.rf = train(Label ~ . ,
                 data = jet23v2,
                 method = "rf",
                 weights = jet23.weight,
                 ntree = 100,
                 tuneGrid = data.frame(.mtry = mtryValues),
                 importance = TRUE,
                 metric = "AMS",
                 trControl = ctrl)

save(jet23.rf, file = 'jet23_rf_AMSop.Rdata')

# plot(jet23.rf)
# jet23.importance = varImp(jet23.rf, scale = F)
# plot(jet23.importance)
# 
# # library(pROC)
# # jet23.rf.roc = roc(response = jet23.rf$pred$obs,
# #                    predictor = jet23.rf$pred$s)
# # 
# # plot(jet23.rf.roc)

library(ROCR)
jet23.pred = jet23.rf$finalModel$votes[,1]
jet23.ptab = performance(prediction(jet23.pred, jet23.train$Label), 'tpr', 'fpr')
plot(jet23.ptab, colorize = T, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))

# ##############################
# jet23_missing = Higgs.train %>% filter(PRI_jet_num > 1, is.na(DER_mass_MMC))
# jet23_complete = Higgs.train %>% filter(PRI_jet_num > 1, !is.na(DER_mass_MMC))
# 
# summary(jet23_missing)
# summary(jet23_complete)
# 
# for (i in 3:length(jet23_missing)) {
#   print(names(jet23_missing[i]))
#   print(t.test(jet23_missing[i], jet23_complete[i]))
# }
# 
# t.test(jet23_missing[3], jet23_complete[3])
# 
# t.test(jet23_missing$DER_mass_transverse_met_lep, jet23_complete$DER_mass_transverse_met_lep)
# t.test(jet23_missing$DER_mass_vis, jet23_complete$DER_mass_vis)
# t.test(jet23_missing$DER_pt_h, jet23_complete$DER_pt_h)
# 
# 
# summary(aov(DER_mass_MMC ~ PRI_jet_num, data = Higgs.train))
# summary(aov(DER_mass_MMC ~ PRI_jet_num, data = Higgs.test))
# 
# group1.train = Higgs.train %>% filter(PRI_jet_num > 1)
# group1.test = Higgs.test %>% filter (PRI_jet_num > 1)
# 
# 
# t.test(group1.test$DER_mass_vis, group1.train$DER_mass_vis)


#jet23.roc = roc(response = jet23.cv$pred$obs,
#                predictor = jet23.cv$pred$s)


#plot(jet23.roc)

#jet23.testpred = predict(jet23.cv, train, type="prob")



#

# Higgs.imp = Higgs.train
# 
# for(i in 1:ncol(Higgs.train)){
#   if(any(is.na(Higgs.train[i]))){
#     Higgs.imp[i] = impute(Higgs.train[i], 'random')
#   }
# }
# 
# Higgs.imp$Label = as.integer(Higgs.imp$Label) - 1
# sum(complete.cases(Higgs.imp)) #250000
# 
# # library(doSNOW)
# # cl <- makeCluster(30, outfile="")
# # registerDoSNOW(cl)
# 
# logit.overall = glm(Label ~ . - Label - Weight,
#                     family = "binomial",
#                     data = Higgs.imp)
# 
# scatter.smooth(logit.overall$fit,
#                residuals(logit.overall, type = "deviance"),
#                lpars = list(col = "red"),
#                xlab = "Fitted Probabilities",
#                ylab = "Deviance Residual Values",
#                main = "Residual Plot for\nLogistic Regression of CERN Data")
# abline(h = 0, lty = 2)
# 
# library(car)
# #influencePlot(logit.overall)
# 
# summary(logit.overall)
# 
# exp(logit.overall$coefficients)
# 
# #library(ROCR)
# #plot(performance(prediction(predict(logit.overall), Higgs.imp$Label), 'tpr', 'fpr'))
# 
# library(pROC)
# overall.roc = roc(Higgs.imp$Label ~ predict(logit.overall, type = c('response')))
# plot(overall.roc)

