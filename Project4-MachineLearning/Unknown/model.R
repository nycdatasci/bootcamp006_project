# Random Forest
######################
###  Main Function ###
######################

#############################
####  Preprocessing data ####
#############################
# Read data
library(dplyr)
df = read.csv('./data/vars_table.csv', header = TRUE)
dfTrain <- read.csv('./data/training.csv', header = TRUE)
dfTest <- read.csv('./data/test.csv', header = TRUE)

trainId = dfTrain$EventId
testId = dfTest$EventId

weight <- dfTrain$Weight
labels <- dfTrain$Label

df_vars = df[, -c(1, 2, 14, 18, 19, 20, 26, 37, 38, 39, 40, 41, 42, 47)]
train <- df_vars[c(1:250000), ]
test <- df_vars[c(250001:800000), ]

##########################
####  Build gbm model ####
##########################

library(caret)
###### Check the documentation for the details of those functions
###### https://cran.r-project.org/web/packages/caret/caret.pdf

library(doMC)
registerDoMC(cores = 4)

# Load our customized metric function.
source('helper.R')

###### Setup a 5 fold cross-validation and use AMS as the metric
###### AMS_summary function defined in helper.R
###### Check details here: http://topepo.github.io/caret/training.html#control
ctrl = trainControl(method = "repeatedcv",number = 2,
                    summaryFunction = AMS_summary)

#labels <- ifelse(labels=='s', 1, 0)

####################################
####  Build random forest model ####
####################################
# ensure results are repeatable

set.seed(0)
rfGrid <-  expand.grid(mtry = 2)

m_rf = train(x=train, y=labels, 
             method="rf", weights=weight, 
             verbose=TRUE, trControl=ctrl, 
             metric="AMS", tuneGrid = rfGrid, 
             ntree = 5000)

plot(m_rf)
############################# Test ######################################
rfPred <- predict(m_rf, newdata = test, type = 'prob')
write.csv(rfPred, "randforest_pred.csv", row.names = FALSE)

############################## Train ####################################
rf_train <- predict(m_rf, newdata = train, type = 'prob')
write.csv(rf_train, "randforest_train.csv", row.names = FALSE)

############# ROC ###############
library(pROC)
labels_train <- ifelse(labels == 's', 1, 0)
auc = roc(labels, rf_train[, 2])
plot(auc, print.thres = TRUE)


######################### Get the threshold value from ROC graph ########
threshold = 0.491
predicted <- rep('b', 550000)
predicted[rfPred[,2] >= threshold] <- 's'
predicted <- as.data.frame(predicted)
predicted$index <- c(1:550000)

# tidy the dataset
prob_df <- cbind(prob = rfPred[, 2], predicted)
prob_df_s <- filter(prob_df, predicted == 's')
prob_df_b <- filter(prob_df, predicted == 'b')
prob_df_b$prob = 1 - prob_df_b$prob
prob_df_combine <- rbind(prob_df_s, prob_df_b)
prob_df_combine <- arrange(prob_df_combine, index)[, -3] # remove the index using for memorzing the order

# save the submission result 
weightRank = rank(rfPred[,2], ties.method= "first")
submission = data.frame(EventId = testId, RankOrder = weightRank, 
                        Class = prob_df_combine$predicted, Probability = prob_df_combine$prob)
write.csv(submission, "rf_test.csv", row.names = FALSE)

######################### Get the threshold value from ROC graph ########
threshold = 0.491
predicted_train <- rep('b', 250000)
predicted_train[rf_train[,2] >= threshold] <- 's'
predicted_train <- as.data.frame(predicted_train)
predicted_train$index <- c(1:250000)

# tidy the dataset of ROC
prob_train_df <- cbind(prob = rf_train[, 2], predicted_train)
prob_train_df_s <- filter(prob_train_df, predicted_train == 's')
prob_train_df_b <- filter(prob_train_df, predicted_train == 'b')
prob_train_df_b$prob = 1 - prob_train_df_b$prob
prob_train_df_combine <- rbind(prob_train_df_s, prob_train_df_b)
prob_train_df_combine <- arrange(prob_train_df_combine, index)[, -3] # remove the index using for memorzing the order

# save the result of train
weightRank_train = rank(rf_train[,2], ties.method= "first")
submission_train = data.frame(EventId = trainId, RankOrder = weightRank_train, 
                              Class = prob_train_df_combine$predicted_train, Probability = prob_train_df_combine$prob)

write.csv(submission_train, 'rf_train.csv', row.names = FALSE)
