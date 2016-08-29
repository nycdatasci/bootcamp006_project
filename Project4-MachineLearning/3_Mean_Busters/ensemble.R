# David Richard Steinmetz
# NYCDSA Project 4 - Kaggle Machine Learning


# Libraries ---------------------------------------------------------------

library(drs)

# Load data ---------------------------------------------------------------

if (!'data.table' %in% loadedNamespaces()) loadlib('data.table')
if (!'xgb' %in% ls()){
  xgb <- as.data.frame(fread('./02_Ben/xgb_ensemble_output.csv'))
  xgb <- xgb[order(xgb$EventId),]
  rownames(xgb) <- NULL
  xgb <- xgb %>% select(EventId, Prob, Class)
}
if (!'logit' %in% ls()){
  logit <- as.data.frame(fread('./02_Ben/logit_ensemble_output.csv'))
  logit <- logit[order(logit$EventId),]
  rownames(logit) <- NULL
  logit <- logit %>% select(EventId, Prob, Class)
}
if (!'rf' %in% ls()){
  rf <- as.data.frame(fread('./03_Sharan/rf_fullfile_training_probs.csv'))
  rf <- rf[order(rf$EventId),]
  rownames(rf) <- NULL
  names(rf) <- c('EventId', 'Class', 'Prob')
  rf <- rf %>% select(EventId, Prob, Class)
}
if (!'train' %in% ls()){
  train <- as.data.frame(fread('../01_External/02_Kaggle/02_CSVs/training.csv'))
}
if(!is.null(xgb$Weight)) xgb$Weight <- train$Weight

# Test structures ---------------------------------------------------------

# lb <- 0
# ub <- 1
# tr <- data.frame(Weight = c(0.002653311, 2.233584487, 2.347388944), 
#                  Class = c('b','b','s'))
# model.names <- c('logit', 'rf', 'xgboost')
# granularity <- 10
# a <- seq(0,1,1/granularity)
# b <- seq(0,1,1/granularity)
# c <- seq(0,1,1/granularity)
# t <- seq(0,1,1/granularity)
# l <- list(a,b,c,t)
# names(l) <- c(model.names, 'threshold')
# g <- expand.grid(l)
# logit <- data.frame(EventID = c(1,2,3), Prob = c(.22,.34,.79))
# rf <- data.frame(EventID = c(1,2,3), Prob = c(.34,.45,.83))
# xgboost <- data.frame(EventID = c(1,2,3), Prob = c(.19,.37,.91))
# z <- list(logit, rf, xgboost)
# names(z) <- model.names
# bounds_type <- ifelse(length(lb)==1, 'repeated', 'unique')


# Supervisor Function -----------------------------------------------------

ensemble <- function(lb, ub, tr, model.names=NULL, granularity=10, ...){
  z <- list(...)  # Create list of model results

  # Checking bounds requirements
  if (missing(lb)) stop('lower bound missing')
  if (missing(ub)) stop('upper bound missing')
  if (missing(tr)) stop('training data missing')
  if (!(class(lb) %in% c('integer','numeric'))) stop('lower bound not numeric vector')
  if (!(class(ub) %in% c('integer','numeric'))) stop('lower bound not numeric vector')
  if (length(lb)!=1 & length(lb)!=length(z)) stop('lower bound length invalid: must be 1 or equal to the number of models')
  if (length(ub)!=1 & length(ub)!=length(z)) stop('upper bound length invalid: must be 1 or equal to the number of models')
  bounds_type <- ifelse(length(lb)==1, 'repeated', 'unique')
  
  # Checking training data requirements
  if (!'data.frame' %in% class(tr)) stop('training data must be in a data frame')
  if (length(tr) < 2) stop('only include the weights and the outcome variable in the training data')
  
  # Checking names requirements
  if (!is.null(model.names) & length(model.names) != length(z)) stop('number of names does not match number of models')
  if (!is.null(model.names) & length(model.names) == length(z)) names(z) <- model.names
  
  # Checking input model requirements
  for (i in 1:length(z)){
    if (!identical(names(z[[1]]), names(z[[i]]))) stop(paste('model df col names do not match in model', i))
    if (nrow(z[[i]])!=nrow(tr)) stop(paste('nrow does not match training data in model', i))
    if (range(z[[i]][,'Prob'])[1] < 0) stop(paste('probs less than 0 in model', i))
    if (range(z[[i]][,'Prob'])[1] > 1) stop(paste('probs more than 1 in model', i))
    if (length(unique(z[[i]][,'EventId'])) != nrow(z[[i]])) stop(paste('non unique EventID in model', i))
  }
  
  # The AMS function defined according to the evaluation page on the website
  AMS <- function(real,pred,weight)
  {
    pred_s_ind = which(pred=="s")                     # Index of s in prediction
    real_s_ind = which(real=="s")                     # Index of s in actual
    real_b_ind = which(real=="b")                     # Index of b in actual
    s = sum(weight[intersect(pred_s_ind,real_s_ind)]) # True positive rate
    b = sum(weight[intersect(pred_s_ind,real_b_ind)]) # False positive rate
    
    b_tau = 10                                        # Regulator weight
    ans = sqrt(2*((s+b+b_tau)*log(1+s/(b+b_tau))-s))
    return(ans)
  }
  
  # Create grid for gridsearch of ensemble parameters
  pseq <- list()
  for (i in 1:length(z)){
    if (bounds_type == 'repeated') pseq[[i]] <- seq(lb, ub, 1/granularity)
    if (bounds_type == 'unique') pseq[[i]] <- seq(lb[i], ub[i], 1/granularity)
  }
  if (!is.null(model.names)) names(pseq) <- model.names
  pseq$threshold <- seq(0, 1, 1/granularity)
  pgrid <- expand.grid(pseq)  # pgrid = parameter grid
  sum_coeff <- rowSums(pgrid[-length(pgrid)])  # Sum of prob coeffs
  pgrid <- pgrid[sum_coeff==1,]  # Prob coeffs must sum to one
  rownames(pgrid) <- 1:nrow(pgrid)

  # Consolidate probabilities
  probs <- z[[1]]['Prob']
  if (length(z)>1){
    for (i in 2:length(z)){
      probs <- cbind(probs, z[[i]]['Prob'])
    }
  }
  names(probs) <- model.names
  
  # Calculate ensembled probabilities
  best_AMS <- 0
  best <- list()
  ams_values <- numeric()
  
  for (i in 1:nrow(pgrid)){  # For each combo of model params and threshold
    print(pgrid[i,])
    for (j in 1:length(probs)){  # For each type of model
      if (j==1){
        res <- matrix(probs[,j]*pgrid[i,j])  # Prob * model weight in ensemble
      } else {
        res <- cbind(res, probs[,j]*pgrid[i,j])
      }
    }
    ens_prob <- rowSums(res)  # Ensemble probabilities
    ens_sig <- ifelse(ens_prob>pgrid[i,length(pgrid)], 's', 'b')  # Signal?
    new_AMS <- AMS(tr$Label, ens_sig, tr$Weight)  # Calc AMS for new model
    cat(paste('new_AMS:', new_AMS, '\n'))
    ams_values[i] <- ifelse(new_AMS==0, 0, new_AMS)
    if (new_AMS > best_AMS){  # Save the new model if it is the best model
      best_AMS <- new_AMS
      best <- list(Probabilities = ens_prob,
                   Parameters = pgrid[i,], 
                   BestAMS = new_AMS)
      cat(paste('Best AMS', best$BestAMS, '\n'))
    }
  }
  
  pgrid_exp <- pgrid
  pgrid_exp$ams <- ams_values
  best$PGrid <- pgrid_exp
  
  # Return model with best AMS score
  return(best)
}
