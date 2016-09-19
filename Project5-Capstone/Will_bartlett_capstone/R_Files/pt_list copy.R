library(dplyr)
library(ggplot2)
library(dtw)
library(dtwclust)
library(TSclust)

load('makeup_clusters.RData')

unique = unique(makeup_clusters$Visit_id)

pt_df_list = list()
for(i in 1:length(unique)){
      df = filter(makeup_clusters, makeup_clusters$Visit_id==unique[i])
      pt_df_list[i] = list(df)
}


time_series = list()
num = 1
for(i in pt_df_list){
      print(class(i))
      ts = i
      ts = arrange(ts, Recorded_time)
      ts$Recorded_time = round(as.POSIXct(ts$Recorded_time), "hour")
      time_end = ts$Recorded_time[nrow(ts)]
      time_1 = ts$Recorded_time[1]
      los = difftime(time_end, time_1, units = "hours")
      los_num = as.numeric(los)
      los_num = los_num + 1
      sequence = seq(from = time_1, by = "hour", length.out = los_num)
      time_col = c(1:los_num)
      gcs_col = c()
      verbal_col = c()
      eye_col = c()
      motor_col = c()
      cluster_col = c()
      visit_col = c()
      for(t in 1:length(sequence)){
            diff = difftime(sequence[t], ts$Recorded_time)
            pos = diff[diff>=0]
            time_ind = which(diff == min(pos))
            df = ts[time_ind,]
            if(nrow(df)>1){df= df[1,]}
            gcs_col[t] = df$`Glasgow Coma Scale Score`[1]
            verbal_col[t] = df$`Best Verbal Response`[1]
            eye_col[t] = df$`Eye Opening`[1]
            motor_col[t] = df$`Best Motor Response`[1]
            cluster_col[t] = df$cluster[1]
            visit_col[t] = df$Visit_id[1]}
      time_series_df = data.frame(time = sequence,
                                   visit_col = visit_col,
                                   entry_num = time_col, 
                                   gcs_col = gcs_col, 
                                   verbal_col = verbal_col, 
                                   eye_col = eye_col,
                                   motor_col = motor_col,
                                   cluster_col = cluster_col)
      time_series[num] = list(time_series_df)
      print(paste('number dfs added =', length(time_series)))
      num = num + 1}


#_______check_______

time_ind_list_t = c()
wrong_t = 0
for(i in 1:2472){
x = time_series[[i]]$visit_col[1]
df = filter(makeup_clusters, makeup_clusters$Visit_id==x)
hours = difftime(round(df$Recorded_time[nrow(df)], "hour"), 
                 round(df$Recorded_time[1], "hour"), 
                 units = "hours")
hours = hours + 1
if(abs(hours - max(time_series[[i]]$entry_num))>0 & nrow(time_series[[i]])>1)
      {wrong_t = wrong_t + 1
      time_ind_list_t = c(time_ind_list_t, i)
      print("WRONG")}
}
wrong_t
time_ind_list_t


wrong_min = 0
time_ind_list_min = c()
for(i in 1:2472){
      x = time_series[[i]]$visit_col[1]
      df = filter(makeup_clusters, makeup_clusters$Visit_id==x)
      minscore = min(df$`Glasgow Coma Scale Score`)
      if(minscore != min(time_series[[i]]$gcs_col)){
            wrong_min = wrong_min + 1
            time_ind_list_min = c(time_ind_list_min, i)}
}
print(paste("number wrong:", wrong_min))
time_ind_list_min



ind = 387
visit_id = time_series[[ind]]$visit_col[1]
df = filter(makeup_clusters, makeup_clusters$Visit_id==visit_id)
View(df)
View(time_series[ind])

