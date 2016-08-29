#####################################
#########Logistic Regression ########
#####################################
#####################################



dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA
testId = dfTest$EventId
trainID = dfTrain$EventId

str(dfTrain)

# dfVar <- read.csv('vars_table.csv',header = T)
# dfVar_1 = dfVar[, -c(1,2,14,18,19,20,26,37,38,39,40,41,42,47)]
# write.csv(dfVar_1, "vars_table_33.csv",row.names=FALSE)

dfVar <- read.csv('vars_table_33.csv',header = T)
train33 <- dfVar[1:250000,]
test33 <- dfVar[250001:800000,]

weight <- dfTrain$Weight
labels <- dfTrain$Label



#find the total percentage of signals and background
table(labels)/length(labels)


sum(complete.cases(train)) # 250000
sum(complete.cases(test)) # 550000

library(doMC)
registerDoMC(cores = 4)


######################################
####  Build glm model for var = 33####
######################################

start.time <- Sys.time()
start.time
logit.overall33 = glm(labels ~ miss_var1+ miss_var5+miss_var6+miss_var7+miss_var13+miss_var24+
                    miss_var25+miss_var26+miss_var27+miss_var28+miss_var29+var2+var3+var4+
                    var8+var9+var10+var11+var12+var14+var15+var16+var17+var18+var19+var20+
                    var21+var22+var23+var30+var1_d+var5_d+var24_d,
                    family = "binomial",
                    data = train33)

end.time <- Sys.time()
end.time
end.time-start.time

pchisq(logit.overall33$deviance, logit.overall33$df.residual, lower.tail = FALSE)

summary(logit.overall33)


####################
scatter.smooth(logit.overall33$fit,
               residuals(logit.overall33, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Labels")
abline(h = 0, lty = 2)

library(car)
influencePlot(logit.overall33)
plot(logit.overall33)

#Residual plot for logistic regression with an added loess smoother; we would 
#hope that, on average, the residual values are 0.


pred33 <- predict(logit.overall33, newdata=train33,type="response")


weightRankTrain33 = rank(pred33, ties.method= "random")

submission33 = data.frame(EventId = trainID, RankOrder = weightRankTrain33, Prob =pred33,Class = labels)
write.csv(submission33, "Logistic_Linear_regression_train_33.csv", row.names=FALSE)




library(pROC)
label1 <- ifelse(labels=='s', 1, 0)
auc = roc(label1, pred33)
plot(auc, print.thres=TRUE)

plot(pred,label1)

#######################
threshold <- 0.339

LGRTestPred33 <- predict(logit.overall33, newdata=test33, type="response")

predicted33 <- rep("b",550000)
predicted33[LGRTestPred33>=threshold] <- "s"
weightRank33 = rank(LGRTestPred33, ties.method= "random")

submission33 = data.frame(EventId = testId, RankOrder = weightRank33, Prob =LGRTestPred33,Class = predicted33)
write.csv(submission33, "Logistic_Linear_regression_test.csv", row.names=FALSE)

######################################
####  Build glm model for var = 20####
######################################

train20 <- read.csv('train2_imputev1_20varibales_train0.csv',header=T)[,-1]
test20 <- read.csv('train2_imputev1_20varibales_test0.csv',header = T)[,-1]


start.time <- Sys.time()
start.time
logit.overall20 = glm(labels ~ v1+v2+v3+v4+v8+v9+v10
                    +v11+v12+v14+v15+v16+v17+v18
                    +v19+v20+v21+v22+v23+v30,
                    family = "binomial",
                    data = train20)

end.time <- Sys.time()
end.time
end.time-start.time

pchisq(logit.overall20$deviance, logit.overall20$df.residual, lower.tail = FALSE)

#####################################
#########Variables =30 #############
#####################################

train30 <- dfVar[1:250000,-c(31,32,33)]
test30 <- dfVar[250001:800000,-c(31,32,33)]

start.time <- Sys.time()
start.time
logit.overall30 = glm(labels ~ miss_var1+ miss_var5+miss_var6+miss_var7+miss_var13+miss_var24+
                      miss_var25+miss_var26+miss_var27+miss_var28+miss_var29+var2+var3+var4+
                      var8+var9+var10+var11+var12+var14+var15+var16+var17+var18+var19+var20+
                      var21+var22+var23+var30,
                      family = "binomial",
                      data = train30)

end.time <- Sys.time()
end.time
end.time-start.time

pchisq(logit.overall30$deviance, logit.overall30$df.residual, lower.tail = FALSE)

summary(logit.overall30)
pred <- predict(logit.overall30, newdata=train30,type="response")



library(pROC)
label1 <- ifelse(labels=='s', 1, 0)
auc = roc(label1, pred)
plot(auc, print.thres=TRUE)

plot(pred,label1)

#######################
threshold <- 0.336

LGRTestPred30 <- predict(logit.overall30, newdata=test30, type="response")

predicted30 <- rep("b",550000)
predicted30[LGRTestPred30>=threshold] <- "s"
weightRank30 = rank(LGRTestPred30, ties.method= "random")

submission30 = data.frame(EventId = testId, RankOrder = weightRank30, Class = predicted30)
write.csv(submission30, "Logistic_Linear_regression_test_30_var.csv", row.names=FALSE)



AIC(logit.overall20,logit.overall30,logit.overall33)
BIC(logit.overall20,logit.overall30,logit.overall33)
