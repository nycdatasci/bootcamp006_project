### EDA FOR KAGGLE PROJECT

######################
###  LOAD LIBRARIES
######################
library(caret)
library(doMC)
library(pROC)
library(xgboost)
library(methods)
library(mlbench)
library(caretEnsemble)
library(caTools)
library(ggplot2)
require(ggplot2)
library(tabplot)
library(pastecs)

#############################
####  Preprocessing data ####
#############################
source('helper.R')

# Read data and mark 999.0 as NAs
dfTrain = read.csv('training.csv', header=T)
dfTest = read.csv('test.csv', header=T)
dfTrain[dfTrain == -999.0] = NA
dfTest[dfTest == -999.0] = NA
testId = dfTest$EventId

### EDA
EDA = dfTrain
dim(EDA)
class(EDA)
n = names(EDA)
n
str(EDA)
head(EDA)
tail(EDA)
summary(EDA)
stat.desc(EDA)

### MISSING DATA
sum(complete.cases(EDA)) # 68114
sum(is.na(EDA))  # [1] 1,580,052
# PERCENT NAs PER COLUMN
NAcolumns=colSums(is.na(EDA))/nrow(EDA)*100
NAcolumns

### WEIGHT INFO
# AVERAGE WEIGHT FOR SIGNAL
mean(EDA[EDA$Label=="s",32])  # [1] 0.008077657
# AVERAGE WEIGHT FOR BACKGROUND
mean(EDA[EDA$Label=="b",32])  # [1] 2.501018

### VISUAL EDA

### HISTOGRAM FOR WEIGHTS FOR SIGNAL
hist(EDA[EDA$Label=='s',32],
     main="Distribution of Weight\nWhen Outcome Is Signal",
     xlab="Weight",
     ylab="Number of Observations",
     col= c("green"))
abline(v=0.01,col="red")

# HISTOGRAM FOR WEIGHTS FOR BACKGROUND
hist(EDA[EDA$Label=='b',32],
     main="Distribution of Weight\nWhen Outcome is Background",
     xlab="Weight",
     ylab="Number of Observations",
     col= c("blue"))
abline(v=4,col="red")

### DENSITY PLOT
plot(range(0,9), range(0,1), type = "n", xlab = "Weights",
     ylab = "Density",
     main = "Weights For Signal and Background")
lines(density(EDA[EDA$Label=='s',32]),col="red")
lines(density(EDA[EDA$Label=='b',32]),col='blue')
text(7,0.7,"Signal(Red)\n          Background(Blue)")

### TABLE PLOT
tableplot(EDA[c(2:11,33)],sortCol = "Label",
                          fontsize = 7,
                          title = "Standardized Distribution Of Variables\nSorted By Label (Var 2-11)")
tableplot(EDA[c(12:22,33)],sortCol = "Label",
                 fontsize = 7,
                 title = "Standardized Distribution Of Variables\nSorted By Label (Var 12-22)")

tableplot(EDA[c(23:32,33)],sortCol = "Label",
                 fontsize = 7,
                 title = "Standardized Distribution Of Variables\nSorted By Label (Var 23-32)")

### CORRELATION PLOT
corEDA=cor(EDA[-c(1,32,33)],use="pairwise.complete.obs") # To Handle NA
library(corrplot)
corrplot(corEDA,method = "ellipse", bg="grey",
          title="Correlation Matrix Plot",
          tl.cex = .5,
          tl.col = "blue")
        
barplot(table(EDA[,33]),
         xlab="Outcome",ylab="Number of Observations",
         main="Frequency Distribution Of Outcome",
         names.arg=c("Background","Signal"),
         col= c("blue","green"))
