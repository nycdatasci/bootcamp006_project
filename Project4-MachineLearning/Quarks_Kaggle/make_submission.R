######################
###  Main Function ###
######################


#############################
####  Preprocessing data ####
#############################

# Read data and mark 999.0 as NAs

setwd("C:/Users/Samriddhi/Desktop/NYCDataScience/MachineLearning/HiggsBoson")

dfTrain <- read.csv('./training/training.csv', header=T, stringsAsFactors = F)
dfTest <- read.csv('./test/test.csv', header=T, stringsAsFactors = F)

dfTrain[dfTrain==-999.0] <- NA  # keep NA instead -9999
dfTest[dfTest==-999.0] <- NA

View(dfTrain)
summary(dfTrain)

testId = dfTest$EventId

str(dfTrain)

weight <- dfTrain$Weight
labels <- dfTrain$Label

train <- dfTrain[, -c(1,32,33)] # remove column
test <- dfTest[,-1]

sum(complete.cases(train)) # 68114
sum(is.na(train))  # 580052

# output is either b or s 
# logistic regression s = 1 or b = 0? what are you gonna do ?

# is it good to impute ? numeric column random, average....if categorical--random, distrubution from that column

# for most kaggle comp, its better to use tree model. if u use median, mean impute, u cannot guarantee....better to start from tree model, xgboost

# dont remove the columns...dont drop......not good to drop information
# keep it to the end of the competition


######## Only about a quarter of the dataset are complete cases, so how do we impute those missing values?
######## Most tree models can handle missing values automatically. Check the following links to see how random forest and gbm
######## handle missing values in the training set.
######## http://stackoverflow.com/questions/14718648/r-gbm-handling-of-missing-values
######## http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#missing1


##########################
####  Build gbm model ####
##########################

library(caret)
# collection of all ML package thatu leanned in 2 weeks.. higher level of function....tuning
# cross validation,  candidate for training parameter.....it has collection of all parameter


###### Check the documentation for the details of those functions
###### https://cran.r-project.org/web/packages/caret/caret.pdf

####### Library used for parallel processing # r is single thread..its lil slow
# complicated model takes more time.

####### Check details here: http://topepo.github.io/caret/parallel.html
####### Windows users please follow the instruction on stackoverflow: http://stackoverflow.com/a/24655923

# 
# require(devtools)
# install_github("doMC")
library(doSNOW)
cl <- makeCluster(30, outfile="")
registerDoSNOW(cl)



# install.packages("doMC")        for mac only
# library(doMC)
# registerDoMC(cores = 4)

# Load our customized metric function.


source('C:/Users/Samriddhi/Desktop/NYCDataScience/MachineLearning/HiggsBoson/Kaggle_Jumpstart/Kaggle_Jumpstart/helper.R')

###### Setup a 5 fold cross-validation and use AMS as the metric
###### AMS_summary function defined in helper.R
###### Check details here: http://topepo.github.io/caret/training.html#control
ctrl = trainControl(method = "repeatedcv",number = 2,
                    summaryFunction = AMS_summary)  # cross validation  k = 10 rule of thumb

###### Setup grid search parameters. 
###### The more candidates you provide, the longer it will take to train the model.
gbmGrid <-  expand.grid(interaction.depth = c(4,6,8), n.trees =(2:8)*100,
                        shrinkage = c(0.01, 0.005, 0.001),
                        n.minobsinnode = c(100, 500, 2000))

# gbmGrid <-  expand.grid(interaction.depth = 4, n.trees =(2)*100,
#                         shrinkage = 0.01,
#                         n.minobsinnode = 100)


###### Train the gbm model
###### If you want to use the gbmGrid you defined above, you could simply set tuneGrid = gbmGrid in the train function.
m_gbm = train(x=train, y=labels, 
              method="gbm", weights=weight, 
              verbose=TRUE, trControl=ctrl, metric="AMS")



###### You can think of this model as a logistic regression. For a logistic regression, we need to find the best threshold for ROC and AUC.
# 0.5 might not be good nummber for threshold

###### Check the definition of ROC and AUC here: 
###### http://thestatsgeek.com/2014/05/05/area-under-the-roc-curve-assessing-discrimination-in-logistic-regression/
gbmTrainPred <- predict(m_gbm, newdata=train, type="prob")

# install.packages("pROC")
library(pROC)
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, gbmTrainPred[,2])
plot(auc, print.thres=TRUE)

# sensitiviy vs specifiicty.....graph

######## From the graph, we can tell the best threshold is 0.002
threshold <- 0.002

gbmTestPred <- predict(m_gbm, newdata=test, type="prob")

predicted <- rep("b",550000)
predicted[gbmTestPred[,2]>=threshold] <- "s"
weightRank = rank(gbmTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "gbm_submission.csv", row.names=FALSE) # gbm_shrink_ntrees_500_submission.csv

####################################
####  Build random forest model ####
####################################

###### The only thing you need to change is the name of method.
###### Check all the available algorithms by typing names(getModelInfo())
###### Check avaliable tuning parameters here: http://topepo.github.io/caret/modelList.html
rfGrid <-  expand.grid(mtry = c(3,6,9))

m_rf = train(x=train, y=labels, 
             method="rf", weights=weight, 
             verbose=TRUE, trControl=ctrl, metric="AMS")


######################
#####   Xgboost  #####
######################

###### Check the source code here: https://github.com/dmlc/xgboost/tree/master/demo/kaggle-higgs
###### The slides I found quite useful to understand xgboost: https://homes.cs.washington.edu/~tqchen/pdf/BoostedTree.pdf
###### The offical paper of xgboost: https://arxiv.org/pdf/1603.02754v2.pdf



####################################
#####   Where to go from here  #####
####################################


######## 1. Feature engineering
######## It is a huge topic but you can get some idea from the winning solution: https://github.com/phunterlau/kaggle_higgs




######## 2. Missing values
######## Tree models like random forest or gbm could handle missing values automatically. So does xgboost.
######## What if you just want to build a simple logistic regression?

######## 3. Ensemble method
######## How to make you final model works better by ensembling couple models?
######## For a classification problem, a majority vote would be a straight-forward approach.
######## Check out this tutorial for a complete guide: http://mlwave.com/kaggle-ensembling-guide/


######## 4. Stacking method
######## This is a more advanced topic. Most of the winning solutions on Kaggle using two-layer stacking method.
######## Check out this tutorial http://machinelearningmastery.com/machine-learning-ensembles-with-r/

