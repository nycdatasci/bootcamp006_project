library(plyr)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(xgboost)
library(Ckmeans.1d.dp)
library(tidyr)
library(iterators)
library(parallel)
library(foreach)
library(doMC)
library(survival)
library(Formula)
library(Hmisc) #missingness imputation
library(rpart)
library(ada)
library(randomForest)

registerDoMC(cores = 8)

setwd("~/Documents/NYCDSA/Project 4")
########################################################
# Load
########################################################
# Load  subsets and clean them
source('Data_cleaning.r')

# Load help functions
source('helper.r')

#######################################################
# Training model list: subset df23
#######################################################

my_control <- trainControl(
  method="cv",
  number= 5,
  repeats = 1,
  savePredictions="final",
  classProbs=TRUE,
  verboseIter  = TRUE,
  index=createResample(df23_im_train$Label, 5),
  summaryFunction=ACC_summary
)


#for df23
df23_model <- caretList(
  make.names(factor(Label))~., 
  data=select(df23_im_train, c(-EventId, -Weight, -PRI_jet_num)),
  trControl=my_control,
  metric="ACC",
  methodList=c("ada", "xgbTree", "rf"),
  
  tuneList=list(
    
    ada = caretModelSpec(method="ada", 
                         tuneGrid=expand.grid(nu = 1, 
                                              maxdepth = 3, 
                                              iter = 5
                                              ),
                          verbose = 2),
    
    xgbtree=caretModelSpec(method='xgbTree', 
                           tuneGrid=expand.grid(eta=0.03, 
                                                max_depth=c(10), 
                                                nrounds=800,
                                                gamma = .1,
                                                min_child_weight=1,
                                                colsample_bytree=1),
                           verbose = 2),   
    
    rf=caretModelSpec(method="rf", tuneGrid=data.frame(mtry=10), verbose = 2)
    
  )
)

save(df23_model, file = "df23_model.RData")

#Check
results <- resamples(df23_model)
summary(results)
dotplot(results)
# correlation between results
modelCor(results)
splom(results)

###########
# Ensemble
###########

# stack using gbm

gbm_ensemble <- caretStack(
  df23_model,
  method="gbm",
  metric="ROC",
  trControl=trainControl(
    method="cv",
    number=5,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction= ACC_summary
  )
)
summary(gbm_ensemble)
ens_gbm_preds <- predict(gbm_ensemble, newdata=df23_im_test,type='prob')


### Subset DTest and calculate the csv file. the ranking must be done when everythinn is toghether
if (exists("dfTest") == F){
  # Check whether the Test data is loaded, otherwise load it and substitute -999
  dfTest = read.csv('test.csv')
  dfTest[dfTest==-999.0] = NA
  print('dfTest loaded!')
}
# impute missing columns for dfTest
df23_TEST = dfTest %>% filter(.,(PRI_jet_num == 2) | (PRI_jet_num == 3) )
df23_TEST$DER_mass_MMC = impute(df23_TEST$DER_mass_MMC, "random")

# Predict
ens_gbm_TEST_preds <- predict(gbm_ensemble, newdata=df23_TEST,type='prob')

# create solution
df_ens_solution = select(df23_TEST, EventId) %>% bind_cols(., 
                                  data.frame('RankOrder' = NA, 
                                  'Class' = ifelse(ens_gbm_TEST_preds <=0.85,'b','s'),
                                'Raw_Probs' = ens_gbm_TEST_preds))

df23_ens_solution = df_ens_solution
save(df23_ens_solution,file = 'df23_ens_final.Rdata')     

load('df0_pred.Rdata')
df0_to_be_merged$Class = ifelse( df0_to_be_merged$Class == 0,'b','s')
colnames(df0_to_be_merged) = names(df23_ens_solution)

# Merging all solutions
df0123_ens_solution = bind_rows(df0_to_be_merged, df23_ens_solution) %>%
                      mutate(.,RankOrder = as.integer(rank(Raw_Probs,ties.method = 'random'))) %>%
                      select(.,-Raw_Probs)

save(df0123_ens_solution,file = 'dfTest_predictions.Rdata')     

# save it to csv
write.csv(df0123_ens_solution, 'dfTest_final_predictions.csv',row.names = FALSE)
