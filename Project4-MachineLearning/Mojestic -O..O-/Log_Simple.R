library(data.table)
library(dplyr)
library(dtplyr)
library(caTools)

# Load data
events <- fread( '../training.csv' )
tests <- fread( '../test.csv' )

x <- events %>% select( -c( EventId, Weight ) )
y <- as.numeric( as.factor( events$Label ) ) - 1

log_mod <- glm( Label ~ ., 
                data = cbind(
                  events %>% select( -c( EventId, Weight, Label ) ),
                  Label = y
                  ),
                family = 'binomial' )

# Check training set AUC
library(ROCR)
rocr_pred <- prediction( log_mod$fitted.values, y, label.ordering = c( 0, 1 ) )
roc.curve <- performance( rocr_pred, measure = 'tpr', x.measure = 'fpr' )
plot(roc.curve)
performance( rocr_pred, 'auc' )@y.values # 0.8159359
 
# Make predictions
id <- tests$EventId
log_pred <- predict( log_mod, newdata = tests,
                     type = 'response')

DT <- data.table( Class = log_pred,
                  EventId = id )

DT_sort <- DT %>% arrange( desc( Class ) ) %>%
  mutate( RankOrder = 1:nrow( DT ),
          Class = ifelse( Class > .5, 's', 'b' ) ) %>%
  arrange( EventId ) %>% 
  select( EventId, RankOrder, Class )

write.csv( DT_sort, file = 'Log_submission_simple.csv', row.names = F)
write.csv( DT, file = 'Log_simple_pred.csv', row.names = F )
