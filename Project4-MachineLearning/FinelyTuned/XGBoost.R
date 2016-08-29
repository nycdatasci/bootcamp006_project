library(xgboost)

##This will be the single dataframe. For the split dataframe, see below.
#Preparing the training datasets for xGboost analysis
train_m = train[,-c(1,32,33)]
train_m = as.matrix(train_m)
labels_x <- ifelse(labels=='s', 1, 0)
labels_x = as.numeric(labels_x)
dtrain <- xgb.DMatrix(data = train_m, label = labels_x)

#Preparing the testing datasets for xGboost analysis
test_m = test[,-1]
test_m = as.matrix(test_m)

#Building the model using xgboost and xgbtrain
watchlist <- list(train=dtrain)
xgbtrain = xgb.train(data = dtrain, max.depth = 9, eta = .01,sub_sample = .9, nround = 3000, objective = "binary:logistic")

#Predicting
pred_gb_pred = predict(xgbtrain, newdata=test_m)

gb_prediction <- ifelse(pred_gb_pred > 0.5, 's', 'b')

weightRank2 = rank(pred_gb_pred, ties.method= "random")

submission2 = data.frame(EventId = testId, RankOrder = weightRank2, Class = gb_prediction)

write.csv(submission2, "xgbtrain_submission_single_new.csv", row.names=FALSE)

##This is the split dataframe
#Preparing the training datasets for xGboost analysis
df_0_m = as.matrix(df_0)
df_1_m = as.matrix(df_1)
df_2_m = as.matrix(df_2)
df_3_m = as.matrix(df_3)

labels_x_0 = ifelse(labels_0 =='s',1,0)
labels_x_1 = ifelse(labels_1 =='s',1,0)
labels_x_2 = ifelse(labels_2 =='s',1,0)
labels_x_3 = ifelse(labels_3 =='s',1,0)

labels_x_0 = as.numeric(labels_x_0)
labels_x_1 = as.numeric(labels_x_1)
labels_x_2 = as.numeric(labels_x_2)
labels_x_3 = as.numeric(labels_x_3)


dtrain_0 = xgb.DMatrix(data = df_0_m, label = labels_x_0)
dtrain_1 = xgb.DMatrix(data = df_1_m, label = labels_x_1)
dtrain_2 = xgb.DMatrix(data = df_2_m, label = labels_x_2)
dtrain_3 = xgb.DMatrix(data = df_3_m, label = labels_x_3)


#Preparing the testing datasets for xGboost analysis

test_m_0 = as.matrix(df_0_test)
test_m_1 = as.matrix(df_1_test)
test_m_2 = as.matrix(df_2_test)
test_m_3 = as.matrix(df_3_test)


#Building the model using xgboost and xgbtrain
watchlist_0 <- list(train=dtrain_0)
watchlist_1 <- list(train=dtrain_1)
watchlist_2 <- list(train=dtrain_2)
watchlist_3 <- list(train=dtrain_3)

bst0 = xgb.train(data = dtrain_0, max.depth = 9, eta = .01,sub_sample = .9, nround = 3000, objective = "binary:logistic")
bst1 = xgb.train(data = dtrain_1, max.depth = 9, eta = .01,sub_sample = .9, nround = 3000, objective = "binary:logistic")
bst2 = xgb.train(data = dtrain_2, max.depth = 9, eta = .01,sub_sample = .9, nround = 3000, objective = "binary:logistic")
bst3 = xgb.train(data = dtrain_3, max.depth = 9, eta = .01,sub_sample = .9, nround = 3000, objective = "binary:logistic")

#Predicting
pred_gb_pred_0 = predict(bst0, newdata=test_m_0)
pred_gb_pred_1 = predict(bst1, newdata=test_m_1)
pred_gb_pred_2 = predict(bst2, newdata=test_m_2)
pred_gb_pred_3 = predict(bst3, newdata=test_m_3)


gb_prediction_0 <- ifelse(pred_gb_pred_0 > 0.5, 's', 'b')
gb_prediction_1 <- ifelse(pred_gb_pred_1 > 0.5, 's', 'b')
gb_prediction_2 <- ifelse(pred_gb_pred_2 > 0.5, 's', 'b')
gb_prediction_3 <- ifelse(pred_gb_pred_3 > 0.5, 's', 'b')

pred_gb_pred_split = c(gb_prediction_0,gb_prediction_1,gb_prediction_2, gb_prediction_3)

weightRank_split = rank(pred_gb_pred_split, ties.method= "random")

submission_split = data.frame(EventId = testId, RankOrder = weightRank_split, Class = pred_gb_pred_split)

write.csv(submission_split, "xgboost_submission_split_new.csv", row.names=FALSE)


