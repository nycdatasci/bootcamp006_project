######################
###  Main Function ###
######################

#############################
####  Preprocessing data ####
#############################

# Read data and mark 999.0 as NAs
dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA
testId = dfTest$EventId

### Delete EventID,weights and Label columns in training dataset
### Delete EventID in testing dataset
train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

## Our data set for analysis
##### training dataset
Joshua_train=subset(train,train$PRI_jet_num==0)
Sam_train=subset(train,train$PRI_jet_num==1)
Chales_train=subset(train,train$PRI_jet_num %in% c(2,3))
Chales_train$PRI_jet_num=as.factor(Chales_train$PRI_jet_num)

##### testing dataset
Joshua_test=subset(test,test$PRI_jet_num==0)
Sam_test=subset(test,test$PRI_jet_num==1)
Chales_test=subset(test,test$PRI_jet_num %in% c(2,3))
Chales_test$PRI_jet_num=as.factor(Chales_test$PRI_jet_num)

#### romove all the unrelavent columns in training dataset

Joshua_train_remove=Joshua_train[,-c(5,6,7,13,23,24,25,26,27,28,29,30)]
Sam_train_remove=Sam_train[,-c(5,6,7,13,23,27,28,29)]
Chales_train_remove=Chales_train


#### remove all the unrelavant columns in the testing dataset

Joshua_test_remove=Joshua_test[,-c(5,6,7,13,23,24,25,26,27,28,29,30)]
Sam_test_remove=Sam_test[,-c(5,6,7,13,23,27,28,29)]
Chales_test_remove=Chales_test


### Imputation and parallel computing (Mac)
library(mice)
library(doMC)
registerDoMC(cores = 4)

########################### Imputation for the training dataset
#### Imputation of training dataset for Joshua
Imput_Joshua_train <- mice(data = Joshua_train_remove, m = 5)
Joshua_MMC_train=data.frame(MMC=apply(Imput_Joshua_train$imp$DER_mass_MMC,1,mean))
## row index
Joshua_train_remove$ID=row.names(Joshua_train_remove)
Joshua_MMC_train$ID=row.names(Joshua_MMC_train)
## replace the missing value
Joshua_train_remove_imputed=Joshua_train_remove
Joshua_train_remove_imputed[Joshua_train_remove$ID %in% Joshua_MMC_train$ID,"DER_mass_MMC"]=Joshua_MMC_train$MMC
summary(Joshua_train_remove_imputed)


#### Imputation of training dataset for Sam
Imput_Sam_train <- mice(data = Sam_train_remove, m = 5)
Sam_MMC_train=data.frame(MMC=apply(Imput_Sam_train$imp$DER_mass_MMC,1,mean))
## row index
Sam_train_remove$ID=row.names(Sam_train_remove)
Sam_MMC_train$ID=row.names(Sam_MMC_train)
## replace the missing value
Sam_train_remove_imputed=Sam_train_remove
Sam_train_remove_imputed[Sam_train_remove$ID %in% Sam_MMC_train$ID,"DER_mass_MMC"]=Sam_MMC_train$MMC
summary(Sam_train_remove_imputed)



#### Imputation of training dataset for Chales
Imput_Chales_train <- mice(data = Chales_train_remove, m = 5)
## fit=with(Imput_Chales_train,lm(DER_mass_MMC~.,Chales_train_remove))
Chales_MMC_train=data.frame(MMC=apply(Imput_Chales_train$imp$DER_mass_MMC,1,mean))
Chales_train_remove$ID=row.names(Chales_train_remove)
Chales_MMC_train$ID=row.names(Chales_MMC_train)
## replace the missing value
Chales_train_remove_imputed=Chales_train_remove
Chales_train_remove_imputed[Chales_train_remove$ID %in% Chales_MMC_train$ID,"DER_mass_MMC"]=Chales_MMC_train$MMC
summary(Chales_train_remove_imputed)



######################### Imputation for the testing dataset
#### Imputation of testing dataset for Joshua
Imput_Joshua_test <- mice(data = Joshua_test_remove, m = 5)
Joshua_MMC_test=data.frame(MMC=apply(Imput_Joshua_test$imp$DER_mass_MMC,1,mean))
## row index
Joshua_test_remove$ID=row.names(Joshua_test_remove)
Joshua_MMC_test$ID=row.names(Joshua_MMC_test)
## replace the missing value
Joshua_test_remove_imputed=Joshua_test_remove
Joshua_test_remove_imputed[Joshua_test_remove$ID %in% Joshua_MMC_test$ID,"DER_mass_MMC"]=Joshua_MMC_test$MMC
summary(Joshua_test_remove_imputed)

#### Imputation of testing dataset for Sam
Imput_Sam_test <- mice(data = Sam_test_remove, m = 5)
Sam_MMC_test=data.frame(MMC=apply(Imput_Sam_test$imp$DER_mass_MMC,1,mean))
## row index
Sam_test_remove$ID=row.names(Sam_test_remove)
Sam_MMC_test$ID=row.names(Sam_MMC_test)
## replace the missing value
Sam_test_remove_imputed=Sam_test_remove
Sam_test_remove_imputed[Sam_test_remove$ID %in% Sam_MMC_test$ID,"DER_mass_MMC"]=Sam_MMC_test$MMC
summary(Sam_test_remove_imputed)

#### Imputation of testing dataset for Chales
Imput_Chales_test <- mice(data = Chales_test_remove, m = 5)
Chales_MMC_test=data.frame(MMC=apply(Imput_Chales_test$imp$DER_mass_MMC,1,mean))
## row index
Chales_test_remove$ID=row.names(Chales_test_remove)
Chales_MMC_test$ID=row.names(Chales_MMC_test)
## replace the missing value
Chales_test_remove_imputed=Chales_test_remove
Chales_test_remove_imputed[Chales_test_remove$ID %in% Chales_MMC_test$ID,"DER_mass_MMC"]=Chales_MMC_test$MMC
summary(Chales_test_remove_imputed)

library(caret)
set.seed(0)
source("helper.R")

library(pROC)

##########################
## GLM, Binomial #########
##########################

names(Sam_train_remove_imputed)
Sam_train_remove_imputed = Sam_train_remove_imputed[ ,-23 ] # deleting id column



Label_sam =dfTrain[rownames(Sam_train_remove_imputed),33] #label b s
weight_sam = dfTrain[rownames(Sam_train_remove_imputed),32]



controlObject2 = trainControl(summaryFunction = AMS_summary,
                              method = "repeatedcv",
                              classProbs = T,
                              repeats = 5,
                              number = 10)


################################## sam ################################################### 
GLM1_Sam = train(x=Sam_train_remove_imputed,y=Label_sam,
                 weights = weight_sam,
                 method = 'glm',
                 family = 'binomial',
                 metric = 'AMS', 
                 trControl = controlObject2)

a=predict(GLM1_Sam,Sam_train_remove_imputed)

confusionMatrix(data = a, reference = as.factor(Label_sam))





####################################33 joshua ###################################33

names(Joshua_train_remove_imputed)

Joshua_train_remove_imputed = Joshua_train_remove_imputed[ , -19 ]

Label_joshua =dfTrain[rownames(Joshua_train_remove_imputed),33] #label b s
weight_joshua = dfTrain[rownames(Joshua_train_remove_imputed),32]



GLM1_Joshua = train( x = Joshua_train_remove_imputed,  y = Label_joshua,
                      weights = weight_joshua,
                     method = 'glm',
                     family = 'binomial',
                     metric = 'AMS', 
                     trControl = controlObject2)

b=predict(GLM1_Joshua,Joshua_train_remove_imputed)
confusionMatrix(data = b, reference = as.factor(Label_joshua))


####################################33 Chales ###################################33
names(Chales_train_remove_imputed)

Chales_train_remove_imputed = Chales_train_remove_imputed[ , -31]

Label_Chales =dfTrain[rownames(Chales_train_remove_imputed),33] #label b s
weight_chales = dfTrain[rownames(Chales_train_remove_imputed),32]



GLM1_Chales = train(x = Chales_train_remove_imputed,  y = Label_Chales,
                    method = 'glm',
                    weights = weight_chales,
                    family = 'binomial',
                    metric = 'AMS', 
                    trControl = controlObject2)

c=predict(GLM1_Chales,Chales_train_remove_imputed)
confusionMatrix(data = c, reference = as.factor(Label_Chales))

## accuracy
(49831+14+74421+40074+246)/250000

library(dplyr)

### For training
ProP_train_Sam=predict(GLM1_Sam,Sam_train_remove_imputed,type = "prob")
ProP_train_Joshua=predict(GLM1_Joshua,Joshua_train_remove_imputed,type = "prob")
ProP_train_Chales=predict(GLM1_Chales,Chales_train_remove_imputed,type = "prob")
complete_ProP_train=rbind.data.frame(ProP_train_Sam,ProP_train_Joshua,ProP_train_Chales)
complete_ProP_train$ID=row.names(complete_ProP_train)
#### order ID
complete_ProP_train_orderID=arrange(complete_ProP_train,as.numeric(ID)) %>%
        select(s)

library(pROC)
labels <- dfTrain$Label
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, complete_ProP_train_orderID[,1])
plot(auc, print.thres=TRUE)

######## From the graph, we can tell the best threshold is 0.002
threshold <- 0.002


#### For Test
Logic_Sam_pred <- predict(GLM1_Sam, newdata=Sam_test_remove_imputed, type="prob")
Logic_Joshua_pred <- predict(GLM1_Joshua,newdata=Joshua_test_remove_imputed, type="prob")
Logic_Chales_pred <- predict(GLM1_Sam, newdata=Chales_test_remove_imputed, type="prob")
complete_Logic_pred=rbind.data.frame(Logic_Sam_pred,Logic_Joshua_pred,Logic_Chales_pred)
complete_Logic_pred$ID=row.names(complete_Logic_pred)

#### order ID
complete_Logic_pred_orderID=arrange(complete_Logic_pred,as.numeric(ID)) %>%
        select(s)

### Make prediction
predicted <- rep("b",550000)
predicted[complete_Logic_pred_orderID[,1]>=threshold] <- "s"
weightRank = rank(complete_Logic_pred_orderID[,1], ties.method= "random")
submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "glm_submission.csv", row.names=FALSE)





