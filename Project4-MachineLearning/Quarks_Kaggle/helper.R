####### The AMS function defined according to the evaluation page on the website
AMS <- function(real,pred,weight)
{
  pred_s_ind = which(pred=="s")                          # Index of s in prediction
  real_s_ind = which(real=="s")                          # Index of s in actual
  real_b_ind = which(real=="b")                          # Index of b in actual
  s = sum(weight[intersect(pred_s_ind,real_s_ind)])      # True positive rate
  b = sum(weight[intersect(pred_s_ind,real_b_ind)])      # False positive rate
  
  b_tau = 10                                             # Regulator weight
  ans = sqrt(2*((s+b+b_tau)*log(1+s/(b+b_tau))-s))
  return(ans)
}


####### Using AMS function defined above as a metrics function for caret
####### Check the details here: http://topepo.github.io/caret/training.html#metrics
AMS_summary <- function(data, lev = NULL, model = NULL){  # data is prediction of our model
  out = (AMS(data$obs, data$pred, data$weights))
  names(out) <- "AMS"
  return(out)
}