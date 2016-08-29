library(corrplot)
library(dplyr)
library(VIM)
library(e1071)
library(dplyr)
library(Hmisc)
library(VIM)
library(psych)
library(flexclust)
library(bestglm)
library(xgboost)

setwd('~/Desktop/Kaggle_higgs/Will/')
load('test_train.RData')
train[train == -999.000] = NA
test[test == -999.000] = NA




#________________creating train datasets____________

df_0 = filter(train, PRI_jet_num == 0)
df_1 = filter(train, PRI_jet_num == 1)
df_2 = filter(train, PRI_jet_num == 2)
df_3 = filter(train, PRI_jet_num == 3)

df_0['DER_mass_MMC'] = impute(df_0['DER_mass_MMC'], "random")
df_1['DER_mass_MMC'] = impute(df_1['DER_mass_MMC'], "random")
df_2['DER_mass_MMC'] = impute(df_2['DER_mass_MMC'], "random")
df_3['DER_mass_MMC'] = impute(df_3['DER_mass_MMC'], "random")

df_0 = df_0[,colSums(is.na(df_0))<nrow(df_0)]
df_1 = df_1[,colSums(is.na(df_1))<nrow(df_1)] 
df_2 = df_2[,colSums(is.na(df_2))<nrow(df_2)]
df_3 = df_3[,colSums(is.na(df_3))<nrow(df_3)]

#_________creating test data sets______________

#Creating Testing Datasets
df_0_test = filter(test, PRI_jet_num == 0)
df_1_test = filter(test, PRI_jet_num == 1)
df_2_test = filter(test, PRI_jet_num == 2)
df_3_test = filter(test, PRI_jet_num == 3)

#Random imputation for testing datasets
df_0_test['DER_mass_MMC'] = impute(df_0_test['DER_mass_MMC'], "random")
df_1_test['DER_mass_MMC'] = impute(df_1_test['DER_mass_MMC'], "random")
df_2_test['DER_mass_MMC'] = impute(df_2_test['DER_mass_MMC'], "random")
df_3_test['DER_mass_MMC'] = impute(df_3_test['DER_mass_MMC'], "random")

#drop missing columns
df_0_test = df_0_test[,colSums(is.na(df_0_test))<nrow(df_0_test)]
df_1_test = df_1_test[,colSums(is.na(df_1_test))<nrow(df_1_test)] 
df_2_test = df_2_test[,colSums(is.na(df_2_test))<nrow(df_2_test)]
df_3_test = df_3_test[,colSums(is.na(df_3_test))<nrow(df_3_test)]

#saving important vectors
testId_0 = df_0_test$EventId
testId_1 = df_1_test$EventId
testId_2 = df_2_test$EventId
testId_3 = df_3_test$EventId
weight_0 <- df_0$Weight
labels_0 <- df_0$Label
weight_1 <- df_1$Weight
labels_1 <- df_1$Label
weight_2 <- df_2$Weight
labels_2 <- df_2$Label
weight_3 <- df_3$Weight
labels_3 <- df_3$Label

#dropping unnecessary columns from dataframes
df_0 <- df_0[, -c(1,20,21,22,23)]
df_0_test <- df_0_test[,-c(1,20,21)]
df_1 <- df_1[, -c(1,20,25,26)]
df_1_test <- df_1_test[,-c(1,20)]
df_2 <- df_2[, -c(1,24,32,33)]
df_2_test <- df_2_test[,-c(1,24)]
df_3 <- df_3[, -c(1,24,32,33)]
df_3_test <- df_3_test[,-c(1,24)]

train <- train[, -c(1,32,33)]
test <- test[,-1]

#________clustering visuals______________

clustering_visuals = function(k){
      zero = as.data.frame(scale(df_0))
      set.seed(0)
      model = kmeans(zero, centers = k, nstart = 20)
      zero$KmeansCluster = model$cluster
      zero$label = labels_0
      y = ceiling(k/3)
      if(k%%3==0){y=y+1}
      tab = table(zero$label)
      par(mfrow = c(y,3))
      plot(zero$label, main = paste("Overall Dist. of S/B", 
                                    "b:", 
                                    round(tab[1]/sum(tab), 3),
                                    " s:", 
                                    round(tab[2]/sum(tab), 3)))
      for(i in 1:k){
            x = filter(zero, KmeansCluster == i)
            perc_b = table(x$label)[1]/sum(table(x$label))
            perc_s = table(x$label)[2]/sum(table(x$label))
            plot(x$label, main = paste("Cluster # ", 
                                       i, 
                                       ", B:", 
                                       round(perc_b, 3), 
                                       ", S: ", 
                                       round(perc_s, 3)))
      }
}
clustering_visuals_1 = function(k){
      zero = as.data.frame(scale(df_1))
      set.seed(0)
      model = kmeans(zero, centers = k, nstart = 20)
      zero$KmeansCluster = model$cluster
      zero$label = labels_1
      y = ceiling(k/3)
      if(k%%3==0){y=y+1}
      tab = table(zero$label)
      par(mfrow = c(y,3))
      plot(zero$label, main = paste("Overall Dist. of S/B", 
                                    "b:", 
                                    round(tab[1]/sum(tab), 3),
                                    " s:", 
                                    round(tab[2]/sum(tab), 3)))
      for(i in 1:k){
            x = filter(zero, KmeansCluster == i)
            perc_b = table(x$label)[1]/sum(table(x$label))
            perc_s = table(x$label)[2]/sum(table(x$label))
            plot(x$label, main = paste("Cluster # ", 
                                       i, 
                                       ", B:", 
                                       round(perc_b, 3), 
                                       ", S: ", 
                                       round(perc_s, 3)))
      }
}
clustering_visuals_2 = function(k){
      zero = as.data.frame(scale(df_2))
      set.seed(0)
      model = kmeans(zero, centers = k, nstart = 20)
      zero$KmeansCluster = model$cluster
      zero$label = labels_2
      y = ceiling(k/3)
      if(k%%3==0){y=y+1}
      tab = table(zero$label)
      par(mfrow = c(y,3))
      plot(zero$label, main = paste("Overall Dist. of S/B", 
                                    "b:", 
                                    round(tab[1]/sum(tab), 3),
                                    " s:", 
                                    round(tab[2]/sum(tab), 3)))
      for(i in 1:k){
            x = filter(zero, KmeansCluster == i)
            perc_b = table(x$label)[1]/sum(table(x$label))
            perc_s = table(x$label)[2]/sum(table(x$label))
            plot(x$label, main = paste("Cluster # ", 
                                       i, 
                                       ", B:", 
                                       round(perc_b, 3), 
                                       ", S: ", 
                                       round(perc_s, 3)))
      }
}
clustering_visuals_3 = function(k){
      zero = as.data.frame(scale(df_3))
      set.seed(0)
      model = kmeans(zero, centers = k, nstart = 20)
      zero$KmeansCluster = model$cluster
      zero$label = labels_3
      y = ceiling(k/3)
      if(k%%3==0){y=y+1}
      tab = table(zero$label)
      par(mfrow = c(y,3))
      plot(zero$label, main = paste("Overall Dist. of S/B", 
                                    "b:", 
                                    round(tab[1]/sum(tab), 3),
                                    " s:", 
                                    round(tab[2]/sum(tab), 3)))
      for(i in 1:k){
            x = filter(zero, KmeansCluster == i)
            perc_b = table(x$label)[1]/sum(table(x$label))
            perc_s = table(x$label)[2]/sum(table(x$label))
            plot(x$label, main = paste("Cluster # ", 
                                       i, 
                                       ", B:", 
                                       round(perc_b, 3), 
                                       ", S: ", 
                                       round(perc_s, 3)))
      }
}

breakdown_all = function(k, split){
      if(split == 0){zero = as.data.frame(scale(df_0))}
      if(split == 1){zero = as.data.frame(scale(df_1))}
      if(split == 2){zero = as.data.frame(scale(df_2))}
      if(split == 3){zero = as.data.frame(scale(df_3))}
      set.seed(0)
      model = kmeans(zero, centers = k, nstart = 20)
      zero$KmeansCluster = model$cluster
      if(split == 0){zero$label = labels_0}
      if(split == 1){zero$label = labels_1}
      if(split == 2){zero$label = labels_2}
      if(split == 3){zero$label = labels_3}
      df = zero%>%
            group_by(KmeansCluster)%>%
            summarise(s = sum(label=="s")/n(), b = sum(label=="b")/n())
      df
}

plot_avg_diff_breakdown_all = function(k, split){
      avg_diff = c()
      for(i in 1:k){
            print(paste("When K =", i))
            bd = breakdown_all(i, split)
            if(split == 0){norm = .745}
            if(split == 1){norm = .643}
            if(split == 2){norm = .489}
            if(split == 3){norm = .696}
            avg_diff[i] = mean(abs(bd$b - norm))
            print(paste("Avg diff =", avg_diff[i]))
      }
      par(mfrow = c(1,1))
      plot(avg_diff, main = paste("Average Cluster Difference from overall S-B distribution for range of Ks, PRI_JET =",
                                  split))
      
}

# par(mfrow = c(2,2))
# x = plot_avg_diff_breakdown_all(12,0)
# y = plot_avg_diff_breakdown_all(12,1)
# z = plot_avg_diff_breakdown_all(12,2)
# q = plot_avg_diff_breakdown_all(12,3)
# par(mfrow = c(2,2))

#add factor variables to zero
add_factors = function(klow, khigh, split){
      if(split == 0){zero = as.data.frame(scale(df_0))}
      if(split == 1){zero = as.data.frame(scale(df_1))}
      if(split == 2){zero = as.data.frame(scale(df_2))}
      if(split == 3){zero = as.data.frame(scale(df_3))}
      for(i in klow:khigh){
            print(paste("K =", i))
            set.seed(0)
            model = kmeans(zero, centers = i, nstart = 20)
            clusters = as.factor(model$cluster)
            zero = cbind(zero, clusters)
            colnames(zero)[ncol(zero)] = i
            print(colnames(zero)[ncol(zero)])
            print(levels(clusters))
      }
      zero
}

# x = add_factors(2, 8, 2)

log_regress = function(df, split){
      log = df
      if(split == 0){log$Label = labels_0}
      if(split == 1){log$Label = labels_1}
      if(split == 2){log$Label = labels_2}
      if(split == 3){log$Label = labels_3}
      zero_logist = glm(Label ~ .,
                        family = "binomial", data = log)
      plot(sort(zero_logist$coefficients),
           main = "Sorted, Scaled Coefficients of Saturated Logistic Regression (minus empty columns)")
      test_predictions = round(zero_logist$fitted.values)
      table = table(truth = log$Label, prediction = test_predictions)
      table
      performance = (table[1] + table[4])/sum(table)
      performance
      majority_guess = sum(table[1], table[3])/sum(table)
      if(split == 2){majority_guess = sum(table[2], table[4])/sum(table)}
      majority_guess
      BIC = BIC(zero_logist)
      AIC = AIC(zero_logist)
      print(paste("BIC =", BIC, 
                  ",  AIC =", AIC, 
                  ",  Performance over the Majority Guess =",  round((performance-majority_guess)*100,5)))
}

log_regress(x, 2)

#_______________________centroids for each dataframe_______________________#


zero = as.data.frame(scale(df_0))
set.seed(0)
model_0 = kmeans(zero, centers = 3, nstart = 20)
centers_df0_k3 = model_0$centers
ncol(centers_df0_k3)
ncol(df_0_test)

one = as.data.frame(scale(df_1))
set.seed(0)
model_1 = kmeans(one, centers = 6, nstart = 20)
centers_df1_k6 = model_1$centers
ncol(centers_df1_k6)
ncol(df_1_test)

two = as.data.frame(scale(df_2))
set.seed(0)
model_2 = kmeans(two, centers = 4, nstart = 20)
centers_df2_k4 = model_2$centers
ncol(centers_df2_k4)
ncol(df_2_test)

three = as.data.frame(scale(df_3))
set.seed(0)
model_3 = kmeans(three, centers = 4, nstart = 20)
centers_df3_k4 = model_3$centers
ncol(centers_df3_k4)
ncol(df_3_test)

#____________scaled_test_dfs__________#

scale_test_0 = scale(df_0_test)
scale_test_1 = scale(df_1_test)
scale_test_2 = scale(df_2_test)
scale_test_3 = scale(df_3_test)

test_clusters_0 = kmeans(scale_test_0, centers_df0_k3, nstart = 20)$cluster
test_clusters_1 = kmeans(scale_test_1, centers_df1_k6, nstart = 20)$cluster
test_clusters_2 = kmeans(scale_test_2, centers_df2_k4, nstart = 20)$cluster
test_clusters_3 = kmeans(scale_test_3, centers_df3_k4, nstart = 20)$cluster

df_0_test$kmeanscluster = as.factor(test_clusters_0)
df_1_test$kmeanscluster = as.factor(test_clusters_1)
df_2_test$kmeanscluster = as.factor(test_clusters_2)
df_3_test$kmeanscluster = as.factor(test_clusters_3)

#_____CHRISTIAN'S CODE WITH NEW VARIABLES_____

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




