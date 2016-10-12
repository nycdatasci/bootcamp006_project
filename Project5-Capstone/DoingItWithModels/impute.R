setwd("C:/Users/ricky/dropbox/bootcamp/capstone/")
test = read.csv("test.final.ames.csv",stringsAsFactors = FALSE)
train = read.csv("train.final.ames.csv",stringsAsFactors = FALSE)

df.miss = rbind(train[,-c(1,70)],test[,-1]) 

library(VIM)
k = round(sqrt(nrow(df.miss)))
imputed.knn = kNN(df.miss, k = k) #Imputing using 1NN.


train1 = cbind("Id" = train$Id,imputed.knn[1:1457,1:68],"SalePrice" = train$SalePrice)
test1 = cbind("Id" = test$Id,imputed.knn[1458:2916,1:68])

write.csv(train1,"train.final17.ames.csv")
write.csv(test1,"test.final17.ames.csv")
