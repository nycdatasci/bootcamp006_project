library(dplyr)
library(VIM)
library(randomForest)

r1 <- read.csv('df_re_1.csv')
r2 <- read.csv('df_re_2.csv')
user_clean <- read.csv('user_clean.csv')
user_clean <- user_clean[, -1] # remove the first column with useless index
train_index <- read.table('./data/train.txt', sep = ',', header = T)
test_index <- read.table('./data/test.txt', sep = ',', header = T)

##### merge data ############
train_mrg <- left_join(user_clean, r1, by = 'user_id') # merge relation  with user_info
train_mrg$age <- as.numeric(train_mrg$age)
train_mrg[, c(3, 6:12, 14, 17:21)] <- lapply(train_mrg[, c(3, 6:12, 14, 17:21)], as.factor)

##### train data ############
# train_mrg <- left_join(train_mrg, r2, by = 'user_id') # merge relation 1, relation 2 with user_info
train <- left_join(train_index, train_mrg, by ='user_id') # build a train dataset
train$lable <- as.factor(train$lable)

# split train dataset into two datasets by product_id(1, 2)
index1 <- which(train$product_id == 1) # index of product 1
train_1 <- train[index1, -c(6)] # type = 1
train_2 <- train[-index1, -c(11, 12, 15:21)] # type = 2
summary(aggr(train_1, prop = T, number = T, gap = T, only.miss = T)) # check the missingness of train_1
summary(aggr(train_2, prop = T, number = T, gap = T, only.miss = T)) # check the missingness of train_2

# check the overlap of user2_id and user_id in train
relat2 <- rename(relation2, user_id = user2_id)
tttestt <- inner_join(train_index, relat2, by = 'user_id')
# 877, it means the number of users in relation2 is too small.

'''
> length(unique(r2$user_id))
[1] 10402
> length(unique(r1$user_id))
[1] 31619
> length(unique(user_clean$user_id))
[1] 38260
> length(unique(train$user_id))
[1] 26000
'''

# check missingness
aggr(train, prop = T, number = F, label = T, gap = T, only.miss = T)
summary(aggr(train_1, prop = T, number = T, gap = T, only.miss = T))

'''
Missings per variable: 
  Variable Count
user_id     0
lable     0
age     0
sex     0
expect_quota     0
max_month_repay 21384
occupation     0
education     0
marital_status     0
live_info     0
local_hk  4616
money_function  4616
company_type     1
salary     1
school_type  4616
flow  4616
gross_profit  4616
business_type  4616
business_year  4617
personnel_num  4616
pay_type  4621
product_id     0
tm_encode     0
nrows_unique     0
nrows     0
num_rel  3122
num_1 18524
num_2 18524
num_3 18524
weight_1 18524
weight_2 18524
weight_3 18524

Missings in combinations of variables: 
  Combinations Count      Percent
0:0:0:0:0:0:0:0:0:0:1:1:0:0:1:1:1:1:1:1:1:0:0:0:0:0:0:0:0:0:0:0   573  2.203846154
0:0:0:0:0:0:0:0:0:0:1:1:0:0:1:1:1:1:1:1:1:0:0:0:0:0:1:1:1:1:1:1  1644  6.323076923
0:0:0:0:0:0:0:0:0:0:1:1:0:0:1:1:1:1:1:1:1:0:0:0:0:1:0:0:0:0:0:0    66  0.253846154
0:0:0:0:0:0:0:0:0:0:1:1:0:0:1:1:1:1:1:1:1:0:0:0:0:1:1:1:1:1:1:1  2333  8.973076923
0:0:0:0:0:1:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0  6242 24.007692308
0:0:0:0:0:1:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:1:1:1:1:1:1 14414 55.438461538
0:0:0:0:0:1:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:1:0:0:0:0:0:0   593  2.280769231
0:0:0:0:0:1:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:1:1:1:1:1:1:1   129  0.496153846
0:0:0:0:0:1:0:0:0:0:0:0:0:0:0:0:0:0:0:0:1:0:0:0:0:0:0:0:0:0:0:0     1  0.003846154
0:0:0:0:0:1:0:0:0:0:0:0:0:0:0:0:0:0:0:0:1:0:0:0:0:0:1:1:1:1:1:1     3  0.011538462
0:0:0:0:0:1:0:0:0:0:0:0:0:0:0:0:0:0:1:0:0:0:0:0:0:0:1:1:1:1:1:1     1  0.003846154
0:0:0:0:0:1:0:0:0:0:0:0:1:1:0:0:0:0:0:0:1:0:0:0:0:1:0:0:0:0:0:0     1  0.003846154
'''

################ random forest #############
############################################


##### Train_1 #####
###### correlation map #######
train_1[is.na(train_1)] <- 0
library(corrplot)
train_1_corr <- as.data.frame(sapply(train_1[, -c(1, 21)], as.numeric))
M_train_1 <- cor(train_1_corr)
corrplot(M_train_1)
library(randomForest)
set.seed(0)


# tree = 500
oob.err1 = numeric(15)
for (mtry in 1:15) {
  fit1 = randomForest(lable ~ .-user_id -product_id, data = train_1, mtry = mtry)
  oob.err1[mtry] = fit1$err.rate[500]
  cat('We are performing iteration', mtry, '\n')
}

plot(1:15, oob.err1, pch = 16, type = 'b',
     xlab = 'Variance Cosidered at Each Split', 
     ylab = 'OOB Mean Squared Error', 
     main = '1_Random Forest OOB Error Rates\nby # of Variables')
# variane important:
varImpPlot(fit1)
oob.err1[1]

# tree = 1000
oob.err2 = numeric(15)
for (mtry in 1:15) {
  fit2 = randomForest(lable ~ .-user_id -product_id, data = train_1, mtry = mtry, ntree = 1000)
  oob.err2[mtry] = fit2$err.rate[500]
  cat('We are performing iteration', mtry, '\n')
}

plot(1:15, oob.err2, pch = 16, type = 'b',
     xlab = 'Variance Cosidered at Each Split', 
     ylab = 'OOB Mean Squared Error', 
     main = '2_Random Forest OOB Error Rates\nby # of Variables')
# variance important:
varImpPlot(fit2) #
oob.err2[1]

# tree = 5000
oob.err3 = numeric(5)
for (mtry in 1:5) {
  fit3 = randomForest(lable ~ .-user_id -product_id, data = train_1, mtry = mtry, ntree = 5000)
  oob.err3[mtry] = fit3$err.rate[500]
  cat('We are performing iteration', mtry, '\n')
}

plot(1:5, oob.err3[1:5], pch = 16, type = 'b',
     xlab = 'Variance Cosidered at Each Split', 
     ylab = 'OOB Mean Squared Error', 
     main = '3_Random Forest OOB Error Rates\nby # of Variables')
# variance important:
varImpPlot(fit3)
oob.err3[1] # [1] 0.3673775

##### best model ######
# mtry = 1
fit0.0 <- randomForest(lable ~ .-user_id -product_id, data = train_1, mtry = 1, ntree = 5000)
fit0.0
varImpPlot(fit0.0)
'''
Call:
 randomForest(formula = lable ~ . - user_id - product_id, data = train_1,      mtry = 1, ntree = 5000) 
Type of random forest: classification
Number of trees: 5000
No. of variables tried at each split: 1

OOB estimate of  error rate: 36.78%
Confusion matrix:
0    1 class.error
0 4152 6697   0.6172919
1 1168 9367   0.1108685
'''

# mtry = 2 # we believe this model is the most appropriate one
fit0 <- randomForest(lable ~ .-user_id -product_id, data = train_1, mtry = 2, ntree = 5000)
fit0
varImpPlot(fit0)

'''
Call:
 randomForest(formula = lable ~ . - user_id - product_id, data = train_1,      mtry = 2, ntree = 5000) 
Type of random forest: classification
Number of trees: 5000
No. of variables tried at each split: 2

OOB estimate of  error rate: 36.92%
Confusion matrix:
0    1 class.error
0 4346 6503   0.5994101
1 1392 9143   0.1321310
'''

# mtry = 3
fit0.1 <- randomForest(lable ~ .-user_id -product_id, data = train_1, mtry = 3, ntree = 5000)
fit0.1
varImpPlot(fit0.1)

'''
Call:
 randomForest(formula = lable ~ . - user_id - product_id, data = train_1,      mtry = 3, ntree = 5000) 
Type of random forest: classification
Number of trees: 5000
No. of variables tried at each split: 3

OOB estimate of  error rate: 37.06%
Confusion matrix:
0    1 class.error
0 4858 5991   0.5522168
1 1933 8602   0.1834836
'''

##### Train_2 #####
summary(aggr(train_2, prop = T, number = T, gap = T, only.miss = T))
###### correlation map #######
train_2 <- train_2[, -c(10, 11, 17)] # remove columns 'live_info' 'company_type' with constant value and 'num_rel'

library(corrplot)
train_2_corr <- as.data.frame(sapply(train_2[, -c(1, 11)], as.numeric)) # remove 'user_id' 'product_id'
M_train_2 <- cor(train_2_corr)
corrplot(M_train_2)

library(randomForest)
set.seed(0)

# tree = 500
oob.err11 = numeric(5)
for (mtry in 1:5) {
  fit11 = randomForest(lable ~ .-user_id -product_id, data = train_2, mtry = mtry)
  oob.err11[mtry] = fit11$err.rate[500]
  cat('We are performing iteration', mtry, '\n')
}

plot(1:5, oob.err11, pch = 16, type = 'b',
     xlab = 'Variance Cosidered at Each Split', 
     ylab = 'OOB Mean Squared Error', 
     main = '11_Random Forest OOB Error Rates\nby # of Variables')
# variane important:
varImpPlot(fit11)
oob.err11[4] # 0.4746534

# tree = 1000
oob.err12 = numeric(10)
for (mtry in 1:10) {
  fit12 = randomForest(lable ~ .-user_id -product_id, data = train_2, mtry = mtry, ntree = 1000)
  oob.err12[mtry] = fit12$err.rate[500]
  cat('We are performing iteration', mtry, '\n')
}

plot(1:10, oob.err12, pch = 16, type = 'b',
     xlab = 'Variance Cosidered at Each Split', 
     ylab = 'OOB Mean Squared Error', 
     main = '2_Random Forest OOB Error Rates\nby # of Variables')
# variance important:
varImpPlot(fit12) 
which(oob.err12 == min(oob.err12))
oob.err12[7] # 0.4753033


# tree = 5000
oob.err13 = numeric(10)
for (mtry in 1:10) {
  fit13 = randomForest(lable ~ .-user_id -product_id, data = train_2, mtry = mtry, ntree = 5000)
  oob.err13[mtry] = fit13$err.rate[500]
  cat('We are performing iteration', mtry, '\n')
}

plot(1:10, oob.err13[1:10], pch = 16, type = 'b',
     xlab = 'Variance Cosidered at Each Split', 
     ylab = 'OOB Mean Squared Error', 
     main = '3_Random Forest OOB Error Rates\nby # of Variables')
# variance important:
varImpPlot(fit13)
which(oob.err13 == min(oob.err13)) # 8
oob.err13[8] # 0.4729203

# tree = 10000
oob.err14 = numeric(10)
for (mtry in 1:10) {
  fit14 = randomForest(lable ~ .-user_id -product_id, data = train_2, mtry = mtry, ntree = 10000)
  oob.err14[mtry] = fit14$err.rate[500]
  cat('We are performing iteration', mtry, '\n')
}

plot(1:10, oob.err14[1:10], pch = 16, type = 'b',
     xlab = 'Variance Cosidered at Each Split', 
     ylab = 'OOB Mean Squared Error', 
     main = '4_Random Forest OOB Error Rates\nby # of Variables')
# variance important:
varImpPlot(fit14)
which(oob.err14 == min(oob.err14)) # 10
oob.err14[10] # 0.472487

# tree = 20000
oob.err15 = numeric(11)
for (mtry in 1:11) {
  fit15 = randomForest(lable ~ .-user_id -product_id, data = train_2, mtry = mtry, ntree = 20000)
  oob.err15[mtry] = fit15$err.rate[500]
  cat('We are performing iteration', mtry, '\n')
}

plot(1:11, oob.err15[1:11], pch = ?, type = 'b',
     xlab = 'Variance Cosidered at Each Split', 
     ylab = 'OOB Mean Squared Error', 
     main = '5_Random Forest OOB Error Rates\nby # of Variables')
# variance important:
varImpPlot(fit15)
which(oob.err15 == min(oob.err15)) # 10
oob.err15[10] # 0.472487


# tree = 20000 set.seed(879)
oob.err16 = numeric(5)
for (mtry in 1:5) {
  fit16 = randomForest(lable ~ .-user_id -product_id, data = train_2, mtry = mtry, ntree = 10000)
  oob.err16[mtry] = fit16$err.rate[500]
  cat('We are performing iteration', mtry, '\n')
}

plot(1:5, oob.err16[1:5], pch = 16, type = 'b',
     xlab = 'Variance Cosidered at Each Split', 
     ylab = 'OOB Mean Squared Error', 
     main = '6_Random Forest OOB Error Rates\nby # of Variables')
# variance important:
varImpPlot(fit16)
which(oob.err16 == min(oob.err16)) # 10
oob.err16[4] # 0.472487


##### best model ######
#...

############# Prediction #################
###### test data ############
test <- left_join(test_index, train_mrg, by = 'user_id') # build a test dataset

# split train dataset into two datasets by product_id(1, 2)
index2 <- which(test$product_id == 1) # index of product 1
test_1 <- test[index2, -c(5)] # type = 1
test_2 <- test[-index2, -c(10, 11, 14:20)] # type = 2
summary(aggr(test_1, prop = T, number = T, gap = T, only.miss = T)) # check the missingness of train_1
summary(aggr(test_2, prop = T, number = T, gap = T, only.miss = T)) # check the missingness of train_2
'''
Missings per variable: 
       Variable Count
user_id     0
age     0
sex     0
expect_quota     0
occupation     0
education     0
marital_status     0
live_info     0
local_hk     0
money_function     0
company_type     0
salary     0
school_type     0
flow     0
gross_profit     0
business_type     0
business_year     0
personnel_num     0
pay_type     2
product_id     0
tm_encode     0
nrows_unique     0
nrows     0
num_rel   299

Missings in combinations of variables: 
Combinations Count     Percent
0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0  7401 96.09192418
0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:1   299  3.88210854
0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:1:0:0:0:0:0     2  0.02596728
'''

'''
Missings per variable: 
        Variable Count
user_id     0
age     1
sex     1
expect_quota     1
max_month_repay     1
occupation     1
education     1
marital_status     1
live_info     1
company_type     1
salary     1
product_id     1
tm_encode     1
nrows_unique     1
nrows     1
num_rel  3222

Missings in combinations of variables: 
Combinations Count     Percent
0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0  1337 29.32660671
0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:1  3221 70.65145865
0:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1     1  0.02193463
'''
###### prediction of test_1 #######
test_1[is.na(test_1)] <- 0
pred_prob1 <- predict(fit0, test_1, type = 'prob')
submit_1 = as.data.frame(cbind(test_1[, 1], pred_prob1))[, c(1, 3)]

###### prediction of test_2 #######
test_2 <- test_2[, -c(9, 10, 16)] # remove columns 'live_info' 'company_type' with constant value and 'num_rel'
test_2[is.na(test_2)] <- 0 # encode 'NA' with 0
# check one user with full missingness
# ...

pred_prob2 <- predict(fitt0, test_2[rownames(test_2) != '9528', ], type = 'prob')
# ...

####### complete prediction #######
submit = rbind(submit_1, submit_2)
# format the file
colnames(submit) <- c('user_id', 'probability') # change the name
submit <- left_join(test_index, submit, by = 'user_id') # change back to the orginal order
write.csv(submit, 'submit.csv') # save to .csv file
write.table(submit, file = 'submit.txt', row.names = F, col.names = T, quote = F, sep = ',')



################ XGboost  ##################
############################################

########## XGBoost with Caret package -> cannot handle missingness, so quit ###########
'''
set.seed(0)
ctrl_1 = trainControl(method = 'repeatedcv', number = 5, verboseIter = T, 
                      summaryFunction = twoClassSummary, classProbs = T)
searchgrid_1 <- expand.grid(nrounds = c(50, 80, 150), max_depth = 9, eta = 0.1, 
                            colsample_bytree = 0.7, gamma = 5, min_child_weight = 0.1)

begin_time = Sys.time()
print(paste0('Starting training time: ', begin_time))
train_1[, 2] <- make.names(train_1[, 2])
xgb_1 = train(x = train_1[, -c(1, 2, 21)], y = train_1[, 2], method = 'xgbTree', 
              tuneGrid = searchgrid, verbose = 2, trControl = ctrl)
end_time = Sys.time()
print(paste0('Training finished at: ', end_time))
print(paste0('Training takes: ', end_time - begin_time))

best_model_1 = xgb_1$bestTune
file_name = paste0('nrounds = ', best_model_1$nrounds, ', max_depth = ', best_model_1$max_depth, 
                   ', eta = ', best_model_1$eta, ', colsample_bytree = ', best_model_1$colsample_bytree, 
                   'gamma = ', best_model_1$gamma, ',min_child_weight = ', best_model_1$min_child_weight)

# ================= save models to images ====================
jpeg(paste0('images/model_paras_', file_name, '.jpg'), width = 800, height = 600)
plot(xgb_1)
dev.off()

xgbPred_1 <- predict(xgb_1, newdata = test_1[, -c(1, 20)], type = 'prob')
'''
########################################################################################
library(dplyr)
library(xgboost)
xgb_data <- sapply(train[, -c(1, 2)], as.numeric)
xgb_train <- xgb.DMatrix(data = xgb_data, label = train[, 2], missing = NA)

xgb_datatest <- sapply(test[, -1], as.numeric)
xgb_test <- xgb.DMatrix(data = xgb_datatest, missing = NA)
                        
xgb_model <- xgb.train(data = xgb_train, max_depth = 9, eta = .01, sub_sample = .9, 
                       nround = 3000, objective = 'binary:logistic')
xgb_pred <- predict(xgb_model, xgb_test)

# transform to submit file
submit_xgb0 <- cbind(test[, 1], xgb_pred)
colnames(submit_xgb1) <- c('user_id', 'probability')
write.table(submit_xgb1, file = 'submit_xgb1.txt', row.names = F, col.names = T, quote = F, sep = ',')

################################# with consumption ####################################
consump <- read.csv('df_con_.csv')

train_consump <- left_join(train, consump, by = 'user_id')
test_consump <- left_join(test, consump, by = 'user_id')

xgb_data1 <- sapply(train_consump[, -c(1, 2)], as.numeric)
xgb_train1 <- xgb.DMatrix(data = xgb_data1, label = train_consump[, 2], missing = NA)

xgb_datatest1 <- sapply(test_consump[, -1], as.numeric)
xgb_test1 <- xgb.DMatrix(data = xgb_datatest1, missing = NA)

xgb_model1 <- xgb.train(data = xgb_train1, max_depth = 9, eta = .01, sub_sample = .9, 
                       nround = 3000, objective = 'binary:logistic')
xgb_pred1 <- predict(xgb_model1, xgb_test1)

# check importance of features
names1 <- dimnames(xgb_data1)[[2]]
importance_matrix1 <- xgb.importance(names1, model = xgb_model1)
xgb.plot.importance(importance_matrix1[1:10, ])

# transform to submit file
submit_xgb1 <- cbind(test_consump[, 1], xgb_pred1)
colnames(submit_xgb1) <- c('user_id', 'probability')
write.table(submit_xgb1, file = 'submit_xgb1.txt', row.names = F, col.names = T, quote = F, sep = ',')



