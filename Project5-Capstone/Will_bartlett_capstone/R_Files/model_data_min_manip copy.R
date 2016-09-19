setwd('~/Desktop/Data_Science/GCS_Folder/Rdata_Files')
load('ts_data.RData')
load('ts_stats.RData')
load('ts_data_min.RData')
library(dplyr)

outcome = ts_stats[,c(1,18)]
model_data_min = ts_data_min[,c('Visit_ID', 'Entry_Num', 'GCS', 'Verbal')]
model_data_min = left_join(model_data_min, outcome)
length(unique(model_data_min$Visit_ID))

# create time at low med and high

dataframe = c()
unique = unique(model_data_min$Visit_ID)
for(i in 1:length(unique)){
      print(i)
      data = filter(model_data_min, Visit_ID==unique[i])
      data = mutate(data, logical_high = GCS>=12 & GCS<=15,
                    logical_mid = GCS>=9 & GCS<=11,
                    logical_low = GCS>=3 & GCS<=8)
      for(i in 1:nrow(data)){
            data$high[i] = nrow(filter(data, Entry_Num<=i &
                                             logical_high==T))
            data$mid[i] = nrow(filter(data, Entry_Num<=i &
                                            logical_mid==T))
            data$low[i] = nrow(filter(data, Entry_Num<=i &
                                            logical_low==T))
      }
      dataframe = rbind(dataframe, data)
}
model_data_min = dataframe
View(model_data)

#create increases/decreases columns

df = data.frame()
unique = unique(model_data_min$Visit_ID)
for(n in 1:length(unique)){
      print(n)
      data = filter(model_data_min, Visit_ID==unique[n])
      data$inc_dec = 0
      data$inc = 0
      data$dec = 0
      if(nrow(data)!=1){
            for(i in 2:nrow(data)){
                  if(data$GCS[i]<data$GCS[i-1]){data$inc_dec[i] = (-1)}
                  if(data$GCS[i]>data$GCS[i-1]){data$inc_dec[i] = 1}
                  data$inc[i] = nrow(filter(data, data$inc_dec==1 &
                                                  Entry_Num<=i))
                  data$dec[i] = nrow(filter(data, data$inc_dec==-1 &
                                                  Entry_Num<=i))
            }
      }
      df = rbind(df, data)
}
View(df)
model_data_min = df

#create trend_3----
trend_df = data.frame()
unique = unique(model_data_min$Visit_ID)
for(n in 1:length(unique)){
      print(n)
      data = filter(model_data_min, Visit_ID==unique[n])
      data$trend = 0 
      for(i in 1:nrow(data)){
            data$trend[i] = data$GCS[1] - data$GCS[i]
            if(data$Entry_Num[i]>3){data$trend[i] = data$GCS[i-3] - data$GCS[i]}
      }
      trend_df = rbind(trend_df, data)
}
model_data_min = trend_df



### RUN TO HERE FOR VISIT ID AND ENTRY NUMBERS TO STAY ----

#get rid of columns, entries with hours above 25
model_data_min = model_data_min[,c(2,3,9,10,11,13,14,5)]
model_data_min = filter(model_data_min, Entry_Num<=25)
model_data_min = mutate(model_data_min, log = GCS*log(Entry_Num))

#name columns
# colnames(model_data_min) = c('hour', 'score', 'time_at_high_scores',
#                              'time_at_middle_scores',
#                              'time_at_low_scores',
#                              'increases', 'decreases',
#                              'last_score', 'log',
#                              'last_score_factor')

#create last_score_as_factor
for(i in 1:nrow(model_data_min)){
      print(i)
      if(model_data_min$last_score[i]>=12){model_data_min$last_score_factor[i]=3}
      else if(model_data_min$last_score[i]<12 & 
              model_data_min$last_score[i]>8){model_data_min$last_score_factor[i]=2}
      else{model_data_min$last_score_factor[i]=1}
}
model_data_min$last_score_factor = as.factor(model_data_min$last_score_factor)

#save(model_data_min, file = 'model_data_min.RData')



