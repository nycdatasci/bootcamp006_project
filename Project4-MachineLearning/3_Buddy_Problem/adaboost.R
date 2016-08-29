

# ensure results are repeatable
set.seed(0)
# load the library
library(caret)
library(class)
# load the dataset
data<-dfTrain[complete.cases(dfTrain),]
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Label~., data=data, method="lvq", trControl=control, tuneLength=5)
# summarize the model
print(model)
plot(model)


##################adaboosting:
dat_tmp<-df0_im_train[,c(2:19, 23)]
formula_df0_im<-as.formula(Label ~ DER_mass_MMC + DER_mass_transverse_met_lep + DER_mass_vis + DER_pt_h + DER_deltar_tau_lep + DER_pt_tot + DER_sum_pt + DER_pt_ratio_lep_tau + DER_met_phi_centrality + PRI_tau_pt + PRI_tau_eta + PRI_tau_phi + PRI_lep_pt + PRI_lep_eta + PRI_lep_phi + PRI_met + PRI_met_phi + PRI_met_sumet)

gbm_algorithm <- gbm(formula_df0_im, data = dat_tmp, distribution = "adaboost", n.trees = 50)
               #cv.folds
gbm_predicted <- predict.gbm(gbm_algorithm, df0_im_test[2:21], n.trees = 50, type = 'response')
table(gbm_predicted, df0_im_test$Label)
table(as.factor(gbm_predicted))
plot(density(gbm_predicted))
gbm_predicted<-plogis(2*gbm_predicted)

########################
library(adaStump)
source(helper.R)

set.seed(0)
fit_with.ada <- ada(formula = formula_df0_im, data = dat_tmp, type = "real",
                    control = rpart.control(maxdepth=1,cp=-1,minsplit=0,xval=0)
                    , iter = 40, nu = 0.05, bag.frac = 1)


fit_ada <- adaStump(formula = formula_df0_im, data = dat_tmp, 
                              type = "real", iter = 40, nu = 0.05, bag.frac = 1)

#pred_ada <- predict(fit_ada, df0_im_test[2:21])
xgb.train.cv = function(train){ 
  ctrl <- trainControl(method = "cv", number = 5,
                       search = "random",summaryFunction = AMS_summary)
  
  ###### Setup grid search parameters. 
  ###### The more candidates you provide, the longer it will take to train the model.
  gbmGrid <-  expand.grid(iter=c(3:10),
                          maxdepth = c(3,5,10), 
                          nu=c(0.01,0.05,0.1))
  
  model.cv = train(y = factor(train$Label), x=train[,-dim(train)[2]], method="ada", trControl=ctrl,  tuneGrid = gbmGrid, 
                      verbose=T,
                      # weights = train$Weight,
                      metric="AMS")
  
  #model.cv <- train(y = factor(train$Label), x = select(train,c(-EventId,-Weight,-Label)),
   #                method = "xgbTree", 
    #              tuneGrid = gbmGrid, 
     #               verbose=TRUE, 
      #              trControl=ctrl,
                    # weights = train$Weight,
       #             metric="AMS")
  
  #save(model.cv, "xgb_cv.RData")
  
  return(model.cv)
}

xgb.train.cv(data_tmp)




