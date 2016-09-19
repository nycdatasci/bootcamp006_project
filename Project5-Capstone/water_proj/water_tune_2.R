#Linlin Cheng
#Proj.5 Tuning file 2

###
#fine tuning after tune1:
###

xgb_grid_2 = expand.grid(
  nrounds = 1000,
  eta = c(0.15, 0.1, 0.09, 0.08, 0.07),
  max_depth = c(8, 9, 10, 11),
  gamma = 1,
  colsample_bytree = c(0.4, 0.5, 0.6),
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
xgb_train_2 = train(
  # data = as.matrix(xgbtrain),
  x = data.matrix(x_train %>% select(-lable)),
  #x = x_train %>% select(-lable),
  #x = model.matrix(lable ~. , data = x_train),
  y = make.names(as.factor(tlabel)),
  # y = make.names(x_train$lable),
  trControl = xgb_trcontrol_2,
  tuneGrid = xgb_grid_2,
  method = "xgbTree"
)

xgb_train_2$method
xgb_train_2$bestTune
#    nrounds max_depth  eta    gamma   colsample_bytree 
# 12 1000        11     0.07     1              0.6
#         min_child_weight
# 12                1
save(xgb_train_2, file = "xgb_tuning2.Rdata")

#############################
#############################
#cross validation with tuned parameters:
param2 <- list(objective = "multi:softmax",
              eval_metric = "merror",
              num_class = 3,
              max_depth = 11,
              eta = 0.07,
              gamma = 1, 
              #subsample = 0.9,
              colsample_bytree = 0.6, 
              min_child_weight = 1
              #max_delta_step = 1
)
cv.nround = 400
cv.nfold = 5
set.seed(1234)
mdcv2 <- xgb.cv(data=xgbtrain, params = param2, nthread=7, 
               nfold=cv.nfold, nrounds=cv.nround,
               verbose = T)

#minimum test error happens at:
which.min(mdcv2$test.merror.mean) #[1] 378

#may plot using the above identified function(2) in ggplot
d <- data.frame(`Min Error` = c( "N.iter = 378", "train = 0.078", "test = 0.193"))

p + annotate("table", x=378, y=0.15, table=d, just=c("left", "top"),
             theme=theme.list(show.box = TRUE, separator = "black",
                              show.csep = TRUE, show.rsep = TRUE, show.colnames=T))
mdcv2 %>%
  select(train.merror.mean, test.merror.mean) %>%
  mutate(IterationNum = 1:400) %>%
  gather(TestOrTrain, merror, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = merror, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + geom_vline(xintercept = which.min(mdcv2$test.merror.mean), linetype="dotted", 
                           color = "blue", size=1)+
  #annotate("table", x=380, y=0.15, table=d, just=c("left", "top"))+
  #+ annotate("text", label = "Footnote", x = 378, y = 0.16, size = 5, colour = "black")+
  theme_bw()


mdcv[which.min(mdcv$test.merror.mean)]
# train.merror.mean train.merror.std test.merror.mean
# 1:          0.078493         0.002426         0.193603
# test.merror.std
# 1:        0.003656

###
######suggest nround = 378, approximately 380
###

###################################################
#for majority vote of three classes, we run the tuned 
#model with 7 different seeds:

xgb_params_3 = list(
  objective = "multi:softmax", 
  num_class = 3,
  eta = 0.07,                                                                  # learning rate
  max.depth = 11, 
  gamma = 1, 
  colsample_bytree = 0.6, 
  min_child_weight = 1,
  eval_metric = "merror"                                                          # evaluation/loss metric
)

xgbtrain <- xgb.DMatrix(data.matrix(select(x_train, -lable)), label=tlabel, missing=NA)


# fit the model with the arbitrary parameters specified above

xgb_1 = xgboost(data = xgbtrain,
                params = xgb_params_3,                                             
                verbose = TRUE, 
                nrounds = 30,
                print.every.n = 10
                #early.stop.round = 10                                          # stop if no improvement within 10 trees
)

pred_1<-predict(xgb_1, )

