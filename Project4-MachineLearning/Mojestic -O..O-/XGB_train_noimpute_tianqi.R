library(data.table)
library(dplyr)
library(dtplyr)
library(caTools)
library(xgboost)

#### Load data ========================================================
train <- fread( '../training.csv' )

# Split Train & Test, Extract Variables
y <- as.numeric( train$Label == 's' )

x <- train %>% select( -c( Label, Weight, EventId ) ) %>% as.matrix()

weight <- train$Weight * 550000 / nrow( train )
weight_scale <- sum( weight * !y ) / sum( weight * y )

#### Model Training ========================================================
# Prepare data for xgboost
xgb_m_train <- xgb.DMatrix( x , 
                            label = y,
                            weight = weight,
                            missing = -999 )
# Parameter selection using Cross Validation
param <- list( "objective"        = "binary:logitraw",
               "scale_pos_weight" = weight_scale,
               "eval_metric"      = "auc",
               "eval_metric"      = "ams@0.15",
               "nthread"          = 4,
               # Params from CV
               'max_depth' = 6,
               'eta'       = 0.1,
               'subsample' = .9,
               'colsample' = .9
)

# Use 10% random sample as validation watch set
sp_valid <- sample.split( y, SplitRatio = .1 )
m_valid <- xgb.DMatrix( x[ sp_valid, ], 
                        label = y[ sp_valid ],
                        weight = weight[ sp_valid ],
                        missing = -999 )

watchlist <- list( train = xgb_m_train, valid = m_valid )
set.seed( 42 )
xgb_model <- xgb.train( params = param,
                        data = xgb_m_train,
                        nrounds = 2000,
                        watchlist = watchlist )

save( xgb_model, file = 'xgb_model_tianqidemo.RData' )


#===========================================================
xgb_m_train2 <- xgb.DMatrix( x , 
                             label = y,
                             weight = rep( 1, length( y ) ),
                             missing = -999 )
# Parameter selection using Cross Validation
param2 <- list( "objective"        = "binary:logitraw",
               # "scale_pos_weight" = weight_scale,
               "eval_metric"      = "auc",
               "eval_metric"      = "ams@0.15",
               "nthread"          = 4,
               # Params from CV
               'max_depth' = 6,
               'eta'       = 0.05,
               'subsample' = .9,
               'colsample' = .9
)

watchlist2 <- list( train = xgb_m_train2, valid = m_valid )
set.seed( 42 )
xgb_model2 <- xgb.train( params = param2,
                         data = xgb_m_train2,
                         nrounds = 2000,
                         watchlist = watchlist2 )

save( xgb_model2, file = 'xgb_model_tianqidemo+noweight.RData' )

# ================================================================
# Prepare data for xgboost
xgb_m_train3 <- xgb.DMatrix( x , 
                            label = y,
                            weight = weight,
                            missing = -999 )
# Parameter selection using Cross Validation
param3 <- list( "objective"        = "binary:logitraw",
               "scale_pos_weight" = weight_scale,
               "eval_metric"      = "auc",
               "eval_metric"      = "ams@0.34",
               "nthread"          = 4,
               # Params from CV
               'max_depth' = 6,
               'eta'       = 0.1,
               'subsample' = .9,
               'colsample' = .9
)

watchlist3 <- list( train = xgb_m_train3, valid = m_valid )
set.seed( 42 )
xgb_model3 <- xgb.train( params = param3,
                        data = xgb_m_train3,
                        nrounds = 2000,
                        watchlist = watchlist3 )

save( xgb_model3, file = 'xgb_model_tianqidemo_ams34.RData' )

# =============================================================
xgb_m_train4 <- xgb.DMatrix( x , 
                             label = y,
                             weight = rep( 1, length( y ) ),
                             missing = -999 )
# Parameter selection using Cross Validation
param4 <- list( "objective"        = "binary:logitraw",
                # "scale_pos_weight" = weight_scale,
                "eval_metric"      = "auc",
                "eval_metric"      = "ams@0.34",
                "nthread"          = 4,
                # Params from CV
                'max_depth' = 6,
                'eta'       = 0.05,
                'subsample' = .9,
                'colsample' = .9
)

watchlist4 <- list( train = xgb_m_train4, valid = m_valid )
set.seed( 42 )
xgb_model4 <- xgb.train( params = param4,
                         data = xgb_m_train4,
                         nrounds = 2000,
                         watchlist = watchlist4 )

save( xgb_model4, file = 'xgb_model_tianqidemo+noweight+ams34.RData' )