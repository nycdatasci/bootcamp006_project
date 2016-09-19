library(dplyr)

load('bad.RData')
load('ts_data.RData')
load('min_score_time.RData')

min_score_time$Recorded_time = as.POSIXct(min_score_time$Recorded_time)
ts_data_min = data.frame()
unique = bad$Visit_id
for(i in 1:length(unique)){
      df = filter(ts_data, ts_data$Visit_ID == unique[i])
      df = arrange(df, Time)
      min_s_time = min_score_time[min_score_time$Visit_id==unique[i],]
      min_s_time = min_s_time$Recorded_time[1]
      df = filter(df, Time >= min_s_time)
      df$Entry_Num = c(1:nrow(df))
      ts_data_min = rbind(ts_data_min, df)
      print(i)}

save(ts_data_min, file = "ts_data_min.RData")

class(ts_data_min$Visit_ID)
class(ts_data$Visit_ID)


