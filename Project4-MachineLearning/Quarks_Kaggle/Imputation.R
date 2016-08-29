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

##### testing dataset
Joshua_test=subset(test,test$PRI_jet_num==0)
Sam_test=subset(test,test$PRI_jet_num==1)
Chales_test=subset(test,test$PRI_jet_num %in% c(2,3))


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

summary(Chales_train_remove)





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




