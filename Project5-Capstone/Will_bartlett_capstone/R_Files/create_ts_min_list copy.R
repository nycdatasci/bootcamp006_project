library(dplyr)
library(ggplot2)

setwd('/Users/williambartlett/Desktop/Data_Science/GCS_Folder/Rdata_files')
load('ts_stats.RData')
load('ts_data_min.RData')
load('centers.RData')
load('time_series_list.RData')


#function that creates list of scores since min score for specified clusters
create_cluster_min_list = function(cluster){
      unique = filter(ts_data_min, Cluster %in% cluster)
      unique = unique(unique$Visit_ID)
      ts_min_list = list()
      for(i in 1:length(unique)){
            scores = filter(ts_data_min, Visit_ID==unique[i])$GCS
            ts_min_list[i] = list(scores)
      }
      names(ts_min_list) = unique
      ts_min_list
}

five_list = create_cluster_min_list(5)




load('ts_min_list')