#Random Forests 

setwd('~/Desktop/Data_Science/GCS_Folder/Rdata_Files')

library(tree)
library(dplyr)
library(MASS)
library(ggplot2)
library(gbm)
library(randomForest)
library(xgboost)

load('model_data_min.RData')

model_data_min = model_data_min[,-c(1,4)]

train_ind = sample(1:nrow(model_data_min), .7*nrow(model_data_min))
train_df = model_data_min[train_ind,]
test_df = model_data_min[-train_ind,]
test_ind = seq(1,nrow(model_data_min), 1)[-train_ind]

set.seed(0)
rf_first25 = randomForest(last_score_factor ~ . , 
                          data = model_data_min, 
                          subset = train_ind, 
                          importance = TRUE)

importance(rf_first25) #visualize variable importance
varImpPlot(rf_first25) #visualize variable importance
summary(rf_first25)

#predictions and errors by Entry_Num

preds = predict(rf_first25, newdata = test_df, type = "response") 
table = table(pred = preds, truth = test_df$last_score_factor)
sum(table[1],table[5], table[9])/sum(table)
test_df$preds = preds


#Check how prediction differs from just guessing current bin. ----
for(i in 1:nrow(test_df)){
      print(i)
      if(test_df$score[i]>=12){test_df$current_bin[i]=3}
      else if(test_df$score[i]<12 & 
              test_df$score[i]>8){test_df$current_bin[i]=2}
      else{test_df$current_bin[i]=1}
}
sum(test_df$current_bin==test_df$last_score_factor)/nrow(test_df)
sum(test_df$preds==test_df$last_score_factor)/nrow(test_df)

test_df = mutate(test_df, current_bin_correct = current_bin == last_score_factor)
current_bin_compare = test_df%>%
      group_by(hour)%>%
      summarize(current_correct = sum(current_bin_correct)/n())

# Visualize RF accuracy by Hours Since Minimum
test_df$preds = preds
test_df = mutate(test_df, correct = preds == last_score_factor)

test_df_summary = test_df%>%
      group_by(hour)%>%
      summarize(percent_true = sum(correct)/n())
View(test_df_summary)

#Random Forest Accuracy 
accuracy_plot = ggplot(data = test_df_summary) + 
      geom_smooth(aes(x = hour, 
                      y = percent_true,
                      color = "Random Forest Accuracy")) +
      ggtitle("Random Forest Accuracy By Hour Since Minimum") + 
      xlab("Hours Since Minimum") +
      ylab("Percent Accurate")
accuracy_plot

#compare plot
compare_plot = ggplot(data = test_df_summary) + 
      geom_smooth(aes(x = hour, 
                      y = percent_true,
                      color = "Random Forest Accuracy")) +
      ggtitle("Comparison of RF and Current Bin Guess Accuracy By Hour Since Minimum") + 
      xlab("Hours Since Minimum") +
      ylab("Percent Accurate") + 
      geom_smooth(data = current_bin_compare, 
                  aes(x = hour, 
                      y = current_correct, 
                      color = 'Current Bin Guess Accuracy'))
compare_plot



