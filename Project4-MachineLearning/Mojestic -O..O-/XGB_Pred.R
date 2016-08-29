load( 'xgb_model_tianqidemo.RData' )
load( 'xgb_model_tianqidemo+noweight.RData' )
test <- fread( '../test.csv' )
id <- test$EventId
true_label <- fread( '../GroundTruth/test_label.csv' )
higgs <- fread( '../GroundTruth/atlas-higgs-challenge-2014-v2.csv' )
# xname <- names( train %>% select( -c( Label, Weight, EventId ) ) )
# importance_matrix <- xgb.importance( xname,
#                                      model = xgb_model )
# xgb.plot.importance( importance_matrix )

xgb_prediction2 <- predict( xgb_model2,
                           xgb.DMatrix(
                             # No label or weight
                             as.matrix( test[ , -'EventId', with = F ] ),
                             missing = -999
                           )
)

xgb_pred <- exp( xgb_prediction2 ) / ( 1 + exp( xgb_prediction2 ) )

threshold <- .85 # % of 's' in train set

DT <- data.table( Class = ifelse( xgb_pred > threshold, 's', 'b' ),
                  EventId = id,
                  prob = xgb_pred )

DT_sort <- DT %>% arrange( desc( prob ) ) %>%
  mutate( RankOrder = 1:nrow( DT ) ) %>%
  arrange( EventId ) %>% 
  select( EventId, RankOrder, Class )

write.csv( DT, file = 'xgb_model_tianqidemo_noweight.csv', row.names = F )





#### Wanna go extreme? ####
# Compare Models
xgb_prediction <- predict( xgb_model,
                           xgb.DMatrix(
                             # No label or weight
                             as.matrix( test[ ,-'EventId', with = F ] ),
                             missing = -999
                            )
)

xgb_prediction2 <- predict( xgb_model2,
                            xgb.DMatrix(
                              # No label or weight
                              as.matrix( test[ , -'EventId', with = F ] ),
                              missing = -999
                            )
)


privateindex <- higgs[ EventId %in% id, KaggleSet ] == 'v'
xgb_pred  <- exp( xgb_prediction )  / ( 1 + exp( xgb_prediction  ) )
xgb_pred2 <- exp( xgb_prediction2 ) / ( 1 + exp( xgb_prediction2 ) )

xresults <- data.table( threshold = seq( 0, 1, .01 ),
                       with_weight = rep( 0, 101 ),
                       no_weight = rep( 0, 101 ) )

weight <- higgs[ KaggleSet == 'v', KaggleWeight ]
LB <- true_label[ privateindex, Label ]

thr <- seq( 0, 1, .01 )
for ( i in 1:length( thr ) ) {
  threshold <- thr[ i ] # % of 's' in train set
  
  Class_1 = ifelse( xgb_pred[ privateindex ]  > threshold, 's', 'b' )
  Class_2 = ifelse( xgb_pred2[ privateindex ] > threshold, 's', 'b' )
  xresults[ i, `:=`( 
    with_weight = AMS( label = LB, pred = Class_1, weight = weight ),
    no_weight   = AMS( label = LB, pred = Class_2, weight = weight )
  ) ]
}

library(ggplot2)
ggplot( data = melt( xresults, id = threshold ) ) +
  geom_line( aes( x = threshold, y = value, color = variable ),
             size = 1 ) +
  scale_x_continuous( breaks = seq( 0, 1, by = .1 ) ) +
  geom_label( aes( 
    x = xresults[ which.max( xresults$with_weight ), threshold ],
    y = max( xresults$with_weight ),
    label = round( max( xresults$with_weight ), 2 )
    ) ) +
  geom_label( aes( 
    x = xresults[ which.max( xresults$no_weight ), threshold ],
    y = max( xresults$no_weight ),
    label = round( max( xresults$no_weight ), 2 )
  ) ) + 
  theme( legend.position = 'top' ) +
  guides( color = guide_legend( title = 'Model' ) )