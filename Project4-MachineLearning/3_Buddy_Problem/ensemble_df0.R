#Linlin Cheng
#ensemble/stack for subset PRI_jet_num == 0:


library(slice)
library(xgboost)
library(ada)
library(dplyr)
library(caret)
library(caretEnsemble)
library(randomForest)
library(rpart)


my_control <- trainControl(
  method="cv",
  number= 5,
  repeats = 1,
  savePredictions="final",
  classProbs=TRUE,
  verboseIter  = TRUE,
  index=createResample(df0_im_train$Label, 5),
  summaryFunction=ACC_summary
)


#for df0
model_list_big <- caretList(
  make.names(factor(Label))~., 
  data=select(df0_im_train, c(-EventId, -Weight, -PRI_jet_num, -PRI_jet_all_pt)),
  trControl=my_control,
  metric="ACC",
  methodList=c("ada", "xgbTree", "rf"),
  
  tuneList=list(
  
  ada = caretModelSpec(method="ada", tuneGrid=expand.grid(nu = 1, 
                                                          maxdepth = 3, 
                                                          iter = 5
                                                          ),
                       verbose = 2),
  
  
  xgbtree=caretModelSpec(method='xgbTree', tuneGrid=expand.grid(eta=0.03, 
                                                                max_depth=c(10), 
                                                                nrounds=800,
                                                                gamma = .1,
                                                                min_child_weight=1,
                                                                colsample_bytree=1),
                                            verbose = 2),   
  rf=caretModelSpec(method="rf", tuneGrid=data.frame(mtry=10), verbose = 2)
  
  )
)

save(model_list_big, file = 'df0_model.Rdata')

#load("~/Downloads/Kaggle_Jumpstart/df0_model.Rdata")


#
gbm_ensemble2 <- caretStack(
  model_list_big,
  method="gbm",
  metric="ROC",
  trControl=trainControl(
    method="cv",
    number=5,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction= twoClassSummary
  )
)


# pred_gbm_stack2<-predict(gbm_ensemble2, newdata=df0_im_test, type = "prob")
# pred_gbm_stack2_class<-ifelse(pred_gbm_stack2>0.65, 1, 0)
# hist(pred_gbm_stack2)
# #save(pred_gbm_stack, file = 'gbm_ensemble_pred.Rdata')
# 
# 
# #arranging for Diego's favorite format:
# df0_to_be_merged<-data.frame(
#   EventID = #specified EVENTID to be extracted
#   Rank = seq(1:length(pred_gbm_stack2)),
#   Class = pred_gbm_stack2_class,
#   Raw_Probs = pred_gbm_stack2
# )
# 
# 

save(gbm_ensemble2, file = 'gbm_emsemble2.Rdata')

pred_test0<-predict(gbm_ensemble2, newdata=dfnew0_im[,c(2:19)], type = "prob")
pred_test0_class1<-ifelse(pred_test0>0.73, 1, 0)
pred_test0_class<-ifelse( pred_test0_class1 == 0,'b','s')
hist(pred_test0)
#save(pred_gbm_stack, file = 'gbm_ensemble_pred.Rdata')


#arranging for Diego's favorite format:
df0_to_be_merged<-data.frame(
  EventId = dfnew0_im$EventId,
  RankOrder = seq(1:length(pred_test0)), #rouge one to be mutated
  Class = pred_test0_class,
  Raw_Probs = pred_test0
)


dim(df0_to_be_merged)
#dim(df0_to_be_merged)
#[1] 220156      4

save(df0_to_be_merged, file ="df0_pred.Rdata")


  
write.table(df0_to_be_merged, file = " df0_to_be_merged.csv", sep = ",")


#########################################################
#tuning for threshold: 
accuracy=c()

for (i in seq(0.2, 1, by = 0.01)){
  pred_tmp<-ifelse(pred_gbm_stack2>i, 1, 0)
  accuracy<-(
    (sum(df0_im_test$Label[which(pred_tmp==1)]==1)+
       (sum(df0_im_test$Label[which(pred_tmp==0)]==0)))/length(pred_gbm_stack)
    )
  print(c(accuracy,i)) #i refers to the cutoff 
}


