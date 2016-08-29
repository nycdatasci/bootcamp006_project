newtrain2 <- read.csv('newsubset23.csv', header=T)
newtrain2$PRI_jet_num=as.factor(newtrain2$PRI_jet_num)
#str(train2)
newsubtrain2 <- newtrain2[, -c(1,32,33,34)]
newsubweight2 <- newtrain2$Weight
newsublabels2 <- newtrain2$Label
library(caret)
library(doMC)
registerDoMC(cores = 4)
source('helper.R')
ctrl = trainControl(method = "repeatedcv",number = 5,
                    summaryFunction = AMS_summary)
gbmGrid <-  expand.grid(interaction.depth =c(3, 4, 5), n.trees =c(200, 500, 800),
                        shrinkage = c(0.1, 0.05, 0.01),
                        n.minobsinnode = c(10, 50, 100))
set.seed(0)
newgbm2 = train(x=newsubtrain2, y=newsublabels2,
                method="gbm", weights=newsubweight2,
                verbose=TRUE, trControl=ctrl,
                metric="AMS",tuneGrid = gbmGrid)
newgbm2
plot(newgbm2)
summary(newgbm2)
df=summary(newgbm2)
gbmTrainPred2<- predict(newgbm2, newdata=newsubtrain2, type="prob")
library(pROC)
labels2 <- ifelse(newsublabels2=='s', 1, 0)
auc2 = roc(labels2, gbmTrainPred2[,2])
plot(auc2, print.thres=TRUE)
threshold <- 0.005

subtest2=read.csv('testsub23.csv', header=T)
names(subtest2)
subtest2[subtest2==-999.0] <- NA
testId2 = subtest2$EventId
subtest2$PRI_jet_num=as.factor(subtest2$PRI_jet_num)
subtest2 <- subtest2[,-c(1,2)]
nrow(subtest2)


gbmTestPred2 <- predict(newgbm2, newdata=subtest2, type="prob")

predicted2 <- rep("b",160128)
predicted2[gbmTestPred2[,2]>=threshold] <- "s"
weightRank2= rank(gbmTestPred2[,2], ties.method= "random")

submission2 = data.frame(EventId = testId2, Class = predicted2)
nrow(submission2)
write.csv(submission2, "gbm_submissionsub2.csv", row.names=FALSE)
# AMS was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 800, interaction.depth = 5, shrinkage = 0.05 and n.minobsinnode = 100. 
summary(newgbm2)
plot(newgbm2)

# run another dataset
train0 <- read.csv('subset0.csv', header=T)
train0$PRI_jet_num=as.factor(train0$PRI_jet_num)
#str(train0)
#unique(train0$PRI_jet_all_pt)
#names(train0)
subtrain0 <- train0[, -c(1,20,21,22,23,24)]
#names(subtrain0)
subweight0 <- train0$Weight
sublabels0 <- train0$Label
library(caret)
library(doMC)
registerDoMC(cores = 4)
source('helper.R')
ctrl0 = trainControl(method = "repeatedcv",number = 5,
                     summaryFunction = AMS_summary)
gbmGrid0 <-  expand.grid(interaction.depth =c(3, 4, 5), n.trees =c(200, 500, 800),
                         shrinkage = c(0.1, 0.05, 0.01),
                         n.minobsinnode = c(10, 50, 100))
set.seed(0)
gbm0 = train(x=subtrain0, y=sublabels0,
             method="gbm", weights=subweight0,
             verbose=TRUE, trControl=ctrl0,
             metric="AMS",tuneGrid = gbmGrid0)
gbm0
plot(gbm0)
gbmTrainPred0<- predict(gbm0, newdata=subtrain0, type="prob")
library(pROC)
labels0 <- ifelse(sublabels0=='s', 1, 0)
auc0 = roc(labels0, gbmTrainPred0[,2])
plot(auc0, print.thres=TRUE)

# AMS was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 800, interaction.depth = 5, shrinkage = 0.1 and n.minobsinnode = 100. 
summary(gbm0)

plot(gbm0)
subtest0=read.csv('testsub0.csv', header=T)
names(subtest0)
subtest0[subtest0==-999.0] <- NA
testId0 = subtest0$EventId
subtest0$PRI_jet_num=as.factor(subtest0$PRI_jet_num)
subtest0 <- subtest0[,-c(1,2,21,22)]
nrow(subtest0)

threshold0 <- 0.001
gbmTestPred0 <- predict(gbm0, newdata=subtest0, type="prob")

predicted0 <- rep("b",220156)
predicted0[gbmTestPred0[,2]>=threshold0] <- "s"
weightRank0= rank(gbmTestPred0[,2], ties.method= "random")

submission0 = data.frame(EventId = testId0, Class = predicted0)
nrow(submission0)
write.csv(submission0, "gbm_submissionsub0.csv", row.names=FALSE)

#improve gbm0
gbmGrid01 <-  expand.grid(interaction.depth =5, n.trees =c(800, 1400, 2000),
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
set.seed(0)
gbm01 = train(x=subtrain0, y=sublabels0,
              method="gbm", weights=subweight0,
              verbose=TRUE, trControl=ctrl0,
              metric="AMS",tuneGrid = gbmGrid01)
gbm01
# The final values used for the model were n.trees = 2000, interaction.depth = 5, shrinkage =
#   0.1 and n.minobsinnode = 100. 
plot(gbm01)
gbmTrainPred01<- predict(gbm01, newdata=subtrain0, type="prob")
library(pROC)
labels0 <- ifelse(sublabels0=='s', 1, 0)
auc01 = roc(labels0, gbmTrainPred01[,2])
plot(auc01, print.thres=TRUE)

gbmGrid02 <-  expand.grid(interaction.depth =5, n.trees =c(2000, 3000, 4000),
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
set.seed(0)
gbm02 = train(x=subtrain0, y=sublabels0,
              method="gbm", weights=subweight0,
              verbose=TRUE, trControl=ctrl0,
              metric="AMS",tuneGrid = gbmGrid02)
gbm02
# The final values used for the model were n.trees = 4000, interaction.depth = 5, shrinkage =
#   0.1 and n.minobsinnode = 100. 
plot(gbm02)
gbmTrainPred02<- predict(gbm02, newdata=subtrain0, type="prob")
library(pROC)
labels0 <- ifelse(sublabels0=='s', 1, 0)
auc02 = roc(labels0, gbmTrainPred02[,2])
plot(auc02, print.thres=TRUE)

threshold02<- 0.001
gbmTestPred02 <- predict(gbm02, newdata=subtest0, type="prob")

predicted02 <- rep("b",220156)
predicted02[gbmTestPred02[,2]>=threshold02] <- "s"
submission02 = data.frame(EventId = testId0, Class = predicted02)


gbmGrid03 <-  expand.grid(interaction.depth =5, n.trees =c(4000, 7000, 10000),
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
set.seed(0)
gbm03 = train(x=subtrain0, y=sublabels0,
              method="gbm", weights=subweight0,
              verbose=TRUE, trControl=ctrl0,
              metric="AMS",tuneGrid = gbmGrid03)
gbm03

# the last run
train1 <- read.csv('subset1.csv', header=T)
train1$PRI_jet_num=as.factor(train1$PRI_jet_num)
names(train1)
subtrain1 <- train1[, -c(1,25,26,27)]
subweight1 <- train1$Weight
sublabels1 <- train1$Label
library(caret)
library(doMC)
registerDoMC(cores = 4)
source('helper.R')
ctrl1 = trainControl(method = "repeatedcv",number = 5,
                     summaryFunction = AMS_summary)
set.seed(0)
gbmGrid1 <-  expand.grid(interaction.depth =c(3, 4, 5), n.trees =c(200, 500, 800),
                         shrinkage = c(0.1, 0.05, 0.01),
                         n.minobsinnode = c(10, 50, 100))
gbm1 = train(x=subtrain1, y=sublabels1,
             method="gbm", weights=subweight1,
             verbose=TRUE, trControl=ctrl1,
             metric="AMS",tuneGrid = gbmGrid1)
gbm1
plot(gbm1)
gbmTrainPred1<- predict(gbm1, newdata=subtrain1, type="prob")
library(pROC)
labels1 <- ifelse(sublabels1=='s', 1, 0)
auc1 = roc(labels1, gbmTrainPred1[,2])
plot(auc1, print.thres=TRUE)


# improve gbm1
set.seed(0)
gbmGrid11 <-  expand.grid(interaction.depth =7, n.trees =800,
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
gbm11 = train(x=subtrain1, y=sublabels1,
              method="gbm", weights=subweight1,
              verbose=TRUE, trControl=ctrl1,
              metric="AMS",tuneGrid = gbmGrid11)
gbm11

summary(gbm11)
# increase depth more
set.seed(0)
gbmGrid12 <-  expand.grid(interaction.depth =c(5,7,10), n.trees =800,
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
gbm12 = train(x=subtrain1, y=sublabels1,
              method="gbm", weights=subweight1,
              verbose=TRUE, trControl=ctrl1,
              metric="AMS",tuneGrid = gbmGrid12)
gbm12
plot(gbm12)
gbmTrainPred12<- predict(gbm12, newdata=subtrain1, type="prob")
library(pROC)
labels1 <- ifelse(sublabels1=='s', 1, 0)
auc12 = roc(labels1, gbmTrainPred12[,2])
plot(auc12, print.thres=TRUE)

set.seed(0)
gbmGrid13 <-  expand.grid(interaction.depth =c(10, 13, 15), n.trees =800,
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
gbm13 = train(x=subtrain1, y=sublabels1,
              method="gbm", weights=subweight1,
              verbose=TRUE, trControl=ctrl1,
              metric="AMS",tuneGrid = gbmGrid13)
gbm13
plot(gbm13)
summary(gbm13)

set.seed(0)
gbmGrid14 <-  expand.grid(interaction.depth =13, n.trees =c(800, 1000),
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
gbm14 = train(x=subtrain1, y=sublabels1,
              method="gbm", weights=subweight1,
              verbose=TRUE, trControl=ctrl1,
              metric="AMS",tuneGrid = gbmGrid14)
gbm14
summary(gbm14)
plot(gbm14)
gbmTrainPred14<- predict(gbm14, newdata=subtrain1, type="prob")
library(pROC)
labels1 <- ifelse(sublabels1=='s', 1, 0)
auc14 = roc(labels1, gbmTrainPred14[,2])
plot(auc14, print.thres=TRUE)

set.seed(0)
gbmGrid15 <-  expand.grid(interaction.depth =13, n.trees =c(1000, 1500, 2000),
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
gbm15 = train(x=subtrain1, y=sublabels1,
              method="gbm", weights=subweight1,
              verbose=TRUE, trControl=ctrl1,
              metric="AMS",tuneGrid = gbmGrid15)
gbm15
plot(gbm15)

gbmTrainPred15<- predict(gbm15, newdata=subtrain1, type="prob")
library(pROC)
labels1 <- ifelse(sublabels1=='s', 1, 0)
auc15 = roc(labels1, gbmTrainPred15[,2])
plot(auc15, print.thres=TRUE)

set.seed(0)
gbmGrid16 <-  expand.grid(interaction.depth = 5, n.trees =c(800, 2000, 5000),
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
gbm16 = train(x=subtrain1, y=sublabels1,
              method="gbm", weights=subweight1,
              verbose=TRUE, trControl=ctrl1,
              metric="AMS",tuneGrid = gbmGrid16)
gbm16

plot(gbm16)
gbmTrainPred16<- predict(gbm16, newdata=subtrain1, type="prob")
library(pROC)
labels1 <- ifelse(sublabels1=='s', 1, 0)
auc16= roc(labels1, gbmTrainPred16[,2])
plot(auc16, print.thres=TRUE)

gbmTrainPred16<- predict(gbm16, newdata=subtrain1, type="prob")
library(pROC)
labels1 <- ifelse(sublabels1=='s', 1, 0)
auc16 = roc(labels1, gbmTrainPred16[,2])
plot(auc16, print.thres=TRUE)

set.seed(0)
gbmGrid17 <-  expand.grid(interaction.depth = 5, n.trees =c(5000, 7500, 10000),
                          shrinkage = 0.1,
                          n.minobsinnode = 100)
gbm17 = train(x=subtrain1, y=sublabels1,
              method="gbm", weights=subweight1,
              verbose=TRUE, trControl=ctrl1,
              metric="AMS",tuneGrid = gbmGrid17)
gbm17

plot(gbm17)

gbmTrainPred17<- predict(gbm17, newdata=subtrain1, type="prob")
library(pROC)
labels1 <- ifelse(sublabels1=='s', 1, 0)
auc17= roc(labels1, gbmTrainPred17[,2])
plot(auc17, print.thres=TRUE)

threshold17 <- 0.007
gbmTestPred17 <- predict(gbm17, newdata=subtest1, type="prob")
predicted17 <- rep("b",169716)
predicted17[gbmTestPred17[,2]>=threshold17] <- "s"
submission17= data.frame(EventId = testId1, Class = predicted17)

subtest1=read.csv('testsub1.csv', header=T)
names(subtest1)
subtest1[subtest1==-999.0] <- NA
testId1 = subtest1$EventId
subtest1$PRI_jet_num=as.factor(subtest1$PRI_jet_num)
subtest1 <- subtest1[,-c(1,2)]
nrow(subtest1)

threshold1 <- 0.002
gbmTestPred1 <- predict(gbm1, newdata=subtest1, type="prob")

predicted1 <- rep("b",169716)
predicted1[gbmTestPred1[,2]>=threshold1] <- "s"
weightRank1= rank(gbmTestPred1[,2], ties.method= "random")

submission1= data.frame(EventId = testId1, Class = predicted1)
nrow(submission1)
write.csv(submission1, "gbm_submissionsub1.csv", row.names=FALSE)

submission=rbind(submission0, submission1, submission2)
submissiondif=rbind(submission02, submission17, submission2)

weightRankdif= rank(c(gbmTestPred02[,2]-0.001, gbmTestPred17[,2]-0.007, gbmTestPred2[,2]-0.005), ties.method= "random")
submissiondif$RankOrder=weightRankdif

nrow(submissiondif)
str(submission)
write.csv(submissiondif, "gbm_submission2.csv", row.names=FALSE)

