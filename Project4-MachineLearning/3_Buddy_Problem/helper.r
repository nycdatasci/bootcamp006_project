# HELPER FUNS #



# Return threshold for the test data (80 train-20 test)
# Shows the accuracy table
print.prediction = function(model,test, type='xgb'){
  
  # pred <- predict(model, as.matrix(test[,2:31]),missing=NA)
  pred.threshold = AMS.cv(model,test,type)
  pred = pred.threshold$pred
  threshold = pred.threshold$best
  
  print('Accuracy table for threshold 0.5')
  tab = table(round(pred), test$Label)
  print(tab)
  
  # Apply threshold
  pred = ifelse(pred < threshold, 0,1)
  # calculate tab
  tab = table(round(pred), test$Label)
  print(tab)
  
  print(paste0('Length = ',length(pred),', accuracy = ', (tab[1,1]+tab[2,2])/sum(tab)))
  return(threshold)
}

#####################################
# Plot and show the importance matrix
#####################################
plot.importance = function(model){
  importance_matrix <- xgb.importance(model = model)
  
  print(importance_matrix)
  xgb.plot.importance(importance_matrix = importance_matrix)
}

##############################################################################
# Load (if needed) the test data, calculates the predicition, the final ranking
# and binds everything into a file which can be submitted to kaggle. Threshold
# for probabilities should be reviewed for each model. Return final dataframe
# Used ONLY for full dataset, not in the ensemble
##############################################################################
make_solution = function(model, threshold, type, comment = 'test'){
  
  # Calculate final predictionsi
  if(type == 'xgb'){
    pred = predict(model, as.matrix(dfTest[,2:31]),missing=NA)}
  if(type=='ada'){ 
    pred = predict(model,select(dfTest,c(-EventId)),type = 'probs')
    pred = pred[,2]}
  if(type=='other'){
    pred = predict(model,select(dTest,c(-EventId, -Weight,-Label)),type = 'prob')}

  # Calculate final ranking
  rank.pred = as.integer(rank(pred,ties.method = 'random'))
  if (length(unique(rank.pred)) != length(pred)){
    return(print('Wrong ranking'))}
  
  # create submission file
  df.solution = select(dfTest, EventId) %>% bind_cols(., 
                  data.frame('RankOrder' = rank.pred, 'Class' = 
                               ifelse(pred <=threshold,'b','s')))
  
  # save it to csv
  write.csv(df.solution, paste0(type,'_',comment,'.csv'),row.names = FALSE)
  
  return(df.solution)
}

###############################################################################
# The AMS function defined according to the evaluation page on the website
################################################################################
AMS <- function(real,pred,weight)
{
  pred_s_ind = which(pred==1)                          # Index of s in prediction
  real_s_ind = which(real==1)                          # Index of s in actual
  real_b_ind = which(real==0)                          # Index of b in actual
  s = sum(weight[intersect(pred_s_ind,real_s_ind)])      # True positive rate
  b = sum(weight[intersect(pred_s_ind,real_b_ind)])      # False positive rate
  
  b_tau = 10                                             # Regulator weight
  ans = sqrt(2*((s+b+b_tau)*log(1+s/(b+b_tau))-s))
  return(ans)
}

#############################################################################
# Using AMS function defined above as a metrics function for caret
# Check the details here: http://topepo.github.io/caret/training.html#metrics
#############################################################################
AMS_summary <- function(data, lev = NULL, model = NULL){
  out = (AMS(data$obs, data$pred, data$weights))
  names(out) <- "AMS"
  return(out)
}

#############################################################################
# Cross validation for AMS metric. Input is test for a given model. 
# The function makes a plot for the AMS = f(threshold) and returns the best value
#############################################################################
AMS.cv = function(model, test, type){
  # threshold range
  threshold = seq(0.5,1,0.05)
  score = threshold
  # Calculate predictions
  if(type=='xgb'){
    pred = predict(model, as.matrix(select(test,c(-EventId,-Weight,-Label))),missing=NA)}
  if(type=='ada'){
    pred = predict(model,select(test,c(-EventId, -Weight,-Label)),type = 'probs')
    pred = pred[,2]}
  if(type=='other'){
    pred = predict(model,select(test,c(-EventId, -Weight,-Label)),type = 'prob')}
  
  for(item in seq(1,length(threshold))){
    tmp = ifelse(pred < threshold[item],0,1)
    score[item] = AMS(test$Label,tmp,test$Weight)
  }
  # print(score)
  best = threshold[score == max(score)]
  plot(x = threshold, y = score)
  print(paste0('Best threshold is ',best))
  
  return(list("pred" = pred, 'best'=best))
}
#############################################################################
# Attempt to make an accuracy metrics that depends on the rounding threshold
#############################################################################
ACC <- function(real,pred){
    ans = (sum(real[which(pred==1)]==1)/length(pred)+
             sum(real[which(pred==0)]==0)/length(pred) )
    return(ans)}
  
  
ACC_summary <- function(data, lev = NULL, model = NULL){
    pred = ifelse(data$pred > 0.5,1,0)
    out = (ACC(data$obs, pred))
    names(out) <- "ACC"
    return(out)}
