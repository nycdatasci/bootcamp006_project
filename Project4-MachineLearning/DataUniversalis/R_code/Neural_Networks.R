###########################################################################
###########################################################################
## Neural Networks Training
# Load in AMS function
setwd("/Users/binfang/Documents/NYCDSA/project/Project_4/codes")
source('helper.R')
source("nnet_plot_update.r")

setwd("/Users/binfang/Documents/NYCDSA/project/Project_4/data")
# dfTrain <- read.csv('subset0.csv', header=T)
# dfTrain <- read.csv('subset1.csv', header=T)
dfTrain <- read.csv('subset23.csv', header=T)
dfTrain <- dfTrain[,]
weight <- dfTrain$Weight
labels <- dfTrain$Label
# train <- dfTrain[, -c(1,20,21,22,23,24)] # subset 0
# train <- dfTrain[, -c(1,20,25,26,27)] # subset 1
train <- dfTrain[, -c(1,2,25,33,34)] # subset 2
nnGrid = expand.grid(decay = c(0.001,0.01,0.1,0.15), size = seq(6,22,2))


# Training
set.seed(1)
ctrl = trainControl(method = "repeatedcv", verboseIter=T, number = 5, 
                    summaryFunction = AMS_summary)
nn_model = train(x=train, y=labels, method="nnet", weights=weight, 
                 tuneGrid=nnGrid, metric = "AMS", verbose=2, 
                 trControl=ctrl, maxit = 500)

nnTrainPred <- predict(nn_model, newdata=train, type="prob")
new_labels <- ifelse(labels=='s', 1, 0)
auc = roc(new_labels, nnTrainPred[,2])
# Auc plot
plot(auc, print.thres=TRUE)

predBin <- ifelse(nnTrainPred[, 2] > 0.007, 1, 0)
predTable <- table(predBin, labels)
accu_sub1 = (predTable[1,1]+predTable[2,2])/(predTable[1,1]+predTable[2,2] + 
                                               predTable[1,2]+predTable[2,1])
nn_best_sub1 = nn_model$bestTune

# setwd("/Users/binfang/Documents/NYCDSA/project/Project_4/results")
# Plot heat map
trellis.par.set(caretTheme())
plot(nn_model, metric = "AMS", plotType = "level") 

# Plot trends
plot(nn_model,circle.cex=7)

#import the function from Github
source("nnet_plot_update.r")
plot.nnet(nn_model,pos.col='blue',neg.col='red',circle.cex=7)

nn_model_sub0 = nn_model

##------------------------------------------------------------------------------
#define correlation matrix for explanatory variables
#define actual parameter values
#define number of variables and observations
set.seed(2)
num.vars <- 30
num.obs <- 250000
cov.mat <- genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma

mod <- train(x = train_test_norm[1:1000, -31], y = as.factor(labels[1:1000,]),
                  method = "nnet", verbose = T, tuneGrid = nnGrid, metric = "AMS",
                  trControl = ctrl, maxit = 100)
mod <- train(x = train_test_norm[1:1000, 1:3], y = as.factor(labels[1:1000,]),
             method = "nnet", verbose = T, tuneGrid = nnGrid, metric = "AMS",
             trControl = ctrl, maxit = 100)
mod <- nnet(Label ~ DER_mass_MMC + DER_mass_transverse_met_lep
            + DER_mass_vis, data = train_test_norm, size = 5)
garson(mod)


###########################################################################
###########################################################################
# NN Training using the best tuning parameters
# Load in AMS function
setwd("/Users/binfang/Documents/NYCDSA/project/Project_4/codes")
source('helper.R')
source("nnet_plot_update.r")

setwd("/Users/binfang/Documents/NYCDSA/project/Project_4/data")
#dfTrain <- read.csv('subset0.csv', header=T)
# dfTrain <- read.csv('subset1.csv', header=T)
dfTrain <- read.csv('subset23.csv', header=T)
dfTrain <- dfTrain[,]
weight <- dfTrain$Weight
labels <- dfTrain$Label
# train <- dfTrain[, -c(1,20,21,22,23,24)] # subset 0
# train <- dfTrain[, -c(1,20,25,26,27)] # subset 1
train <- dfTrain[, -c(1,2,25,33,34)] # subset 2
nnGrid = expand.grid(decay = c(0.1), size = c(22))

# Training
set.seed(1)
ctrl = trainControl(method = "repeatedcv", verboseIter=T, number = 5, 
                    summaryFunction = AMS_summary)
nn_model_best = train(x=train, y=labels, method="nnet", weights=weight, 
                      tuneGrid=nnGrid, metric = "AMS", verbose=2, 
                      trControl=ctrl, maxit = 1000)

nnTrainPred_best <- predict(nn_model_best, newdata=train, type="prob")
new_labels <- ifelse(labels=='s', 1, 0)
auc = roc(new_labels, nnTrainPred_best[,2])
# Auc plot
plot(auc, print.thres=TRUE, main = "hidden size = 9, weight decay = 0.001, accuracy = 80.4%")

predBin_best <- ifelse(nnTrainPred_best[, 2] > 0.003, 1, 0)
predTable_best <- table(predBin_best, labels)
accu_sub1_best = (predTable_best[1,1]+predTable_best[2,2])/(predTable_best[1,1]+predTable_best[2,2] + 
                                                              predTable_best[1,2]+predTable_best[2,1])

setwd("/Users/binfang/Documents/NYCDSA/project/Project_4/codes")
nnTrainPred_best_subset3 <- nnTrainPred_best
save(nnTrainPred_best_subset3,file="best_model.RData")