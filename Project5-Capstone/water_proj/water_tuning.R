#Linlin Cheng
#Proj 5. tuning file1

library(caret)
library(xgboost)
library(readr)
library(dplyr)
library(tidyr)

# load in the training data
df_train = xgbtrain
# xgboost fitting with arbitrary parameters

xgb_params_1 = list(
  objective = "multi:softmax", 
  num_class = 3,
  eta = 0.1,                                                                  # learning rate
  max.depth = 1, 
  eval_metric = "merror"                                                          # evaluation/loss metric
)

xgbtrain <- xgb.DMatrix(data.matrix(select(x_train, -lable)), label=tlabel, missing=NA)


# fit the model with the arbitrary parameters specified above
xgb_1 = xgboost(data = xgbtrain,
                params = xgb_params_1,                                             
                verbose = TRUE, 
                nrounds = 1000,
                print.every.n = 1,
                #early.stop.round = 10                                          # stop if no improvement within 10 trees
)

# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = xgbtrain,
                  nrounds = 1000, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10
)

# plot the AUC for the training and testing samples
xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()


######################################################################
######################################################################
# set up the cross-validated hyper-parameter search
xgb_grid_2 = expand.grid(
  nrounds = c(100, 500, 1000),
  eta = c(0.3, 0.1, 0.01, 0.001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  colsample_bytree = c(0.1, 0.5),
  min_child_weight = 1 
)

# pack the training control parameters
xgb_trcontrol_2 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = multiClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
#   using CV to evaluate

#x_train_1<-na.omit(x_train)
xgb_train_1 = train(
  # data = as.matrix(xgbtrain),
  x = data.matrix(x_train %>% select(-lable)),
  #x = x_train %>% select(-lable),
  #x = model.matrix(lable ~. , data = x_train),
  y = make.names(as.factor(tlabel)),
  # y = make.names(x_train$lable),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

xgb_train_1$method
xgb_train_1$bestTune
#nrounds max_depth eta gamma colsample_bytree min_child_weight
#90    1000        10 0.1     1              0.5                1

save(xgb_train_1, file = "xgb_tuning.Rdata")

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = Mean_ROC, color = Mean_ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

#variable importance plot
plot(varImp(xgb_train_1, scale = FALSE))



#############################
#############################
#cross validation with tuned parameters:
param <- list(objective = "multi:softmax",
              eval_metric = "merror",
              num_class = 3,
              max_depth = 10,
              eta = 0.1,
              gamma = 1, 
              #subsample = 0.9,
              colsample_bytree = 0.5, 
              min_child_weight = 1
              #max_delta_step = 1
)
cv.nround = 1000
cv.nfold = 5
mdcv <- xgb.cv(data=xgbtrain, params = param, nthread=7, 
               nfold=cv.nfold, nrounds=cv.nround,
               verbose = T)

#may plot using the above identified function(2) in ggplot
d <- data.frame(`Min Error` = c( "N.iter = 370", "train = 0.08", "test = 0.19"))

p + annotate("table", x=380, y=0.15, table=d, just=c("left", "top"),
             theme=theme.list(show.box = TRUE, separator = "black",
                              show.csep = TRUE, show.rsep = TRUE, show.colnames=T))
mdcv %>%
  select(train.merror.mean, test.merror.mean) %>%
  mutate(IterationNum = 1:1000) %>%
  gather(TestOrTrain, merror, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = merror, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + geom_vline(xintercept = which.min(mdcv$test.merror.mean), linetype="dotted", 
                           color = "blue", size=1)+
  #annotate("table", x=380, y=0.15, table=d, just=c("left", "top"))+
  + annotate("text", label = "Footnote", x = 380, y = 0.16, size = 5, colour = "black")+
  theme_bw()

which.min(mdcv$test.merror.mean)
#[1] 370
mdcv[which.min(mdcv$test.merror.mean)]

