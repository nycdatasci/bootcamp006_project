setwd('~/Desktop/Data_Science/GCS_Folder/Rdata_Files')
load('ts_data.RData')
load('ts_stats.RData')
library(dplyr)


ggplot(data = model_data, aes(GCS)) + geom_bar()

outcomes = select(ts_stats, Visit_ID, last_score, outcome)
model_data = ts_data[,c('Visit_ID', 'Entry_Num', 'GCS', 'Verbal')]
model_data = left_join(model_data, outcomes)
length(unique(model_data$Visit_ID))

# create time at low med and high

dataframe = c()
unique = unique(model_data$Visit_ID)
for(i in 1:length(unique)){
      print(i)
      data = filter(model_data, Visit_ID==unique[i])
      data = mutate(data, logical_high = GCS>=12 & GCS<=15,
                    logical_mid = GCS>=8 & GCS<=11,
                    logical_low = GCS>=3 & GCS<=7)
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
model_data = dataframe
View(model_data)

model_data = model_data[,-c(7,8,9)]
save(model_data, file = 'model_data.RData')

#create increases/decreases columns

df = data.frame()
unique = unique(model_data$Visit_ID)
for(n in 1:length(unique)){
      print(n)
      data = filter(model_data, Visit_ID==unique[n])
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
model_data = df


#create avg_last_5
avg_last_5 = data.frame()
unique = unique(model_data$Visit_ID)
for(n in 1:length(unique)){
      print(n)
      data = filter(model_data, Visit_ID==unique[n])
      data$last_5_avg = 0 
      for(i in 1:nrow(data)){
            data$last_5_avg[i] = mean(data$GCS[1:i])
            if(data$Entry_Num[i]>=5){data$last_5_avg[i] = mean(data$GCS[(i-4):i])}
      }
      avg_last_5 = rbind(avg_last_5, data)
}
model_data = avg_last_5




