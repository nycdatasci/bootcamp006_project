#____________cluster analysis_______
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(grid)
setwd("~/Desktop/Data_Science/GCS_Folder/Rdata_Files")

load('makeup_stats.RData') 
load('makeup.RData')
load('patients.RData')
load('bad.RData')

# C = 12
# bad = filter(patients, Min_GCS_Score<C)


#__________________________k means clusters
set.seed(1)
clusters_f_l = kmeans(bad[,c(2,3)], centers = 5, n = 100)

#take centers
centers = cbind(clusters_f_l$centers, count = clusters_f_l$size)
centers = data.frame(centers) #put centers into dataframe
View(centers)

save(centers, file = "centers.RData")

# add centers to bad
#bad$cluster = clusters_f_l$cluster


#_________________plot centers_________________

g_cluster = ggplot(centers, aes(x=Min_GCS_Score, y=Last_GCS_Score))
g_cluster + geom_point(aes(size = count))

#________________Clusters Visuals Functions______________

clusters_visuals = function(i){
      bad_k = filter(bad, cluster ==i)
      ymax = max(table(bad_k$Min_GCS_Score), table(bad_k$Last_GCS_Score))
      min_bar = ggplot(data = bad_k, aes(x = Min_GCS_Score)) +
            geom_bar() + ggtitle("Minimum GCS Score") + 
            scale_y_continuous(limits = c(0, ymax))
      last_bar = ggplot(data = bad_k, aes(x = Last_GCS_Score)) +
            geom_bar() + ggtitle("Last GCS Score") +
            scale_y_continuous(limits = c(0, ymax))
      jitter_k = ggplot(data = bad_k, aes(x = jitter(Min_GCS_Score), y = jitter(Last_GCS_Score))) +
            geom_point(alpha = .5) + xlab("Minimum GCS Score") + ylab("Maximum GCS Score")
      grid.arrange(min_bar, last_bar, jitter_k, ncol = 3,
                   top=paste("Cluster #", i, ", Min:", round(centers[i,1],2),"Last:", round(centers[i,2],2)))
}

clusters_visuals(1)




#_______center analysis__________
los = c()
range = c()
sd = c()
i=1
for(i in 1:5){
      bad_x = filter(bad, cluster == i)
      los_i = mean(as.numeric(bad_x$Length_Of_Stay_Seconds), na.rm = T)/3600/24
      bad_breakout = filter(makeup, Visit_id %in% bad_x$Visit_id)%>%
            group_by(Visit_id)%>%
            summarise(range_i = diff(range(`Glasgow Coma Scale Score`)),
                      sd_i = sd(`Glasgow Coma Scale Score`)
            )
      los[i] = los_i
      range[i] = mean(bad_breakout$range_i, na.rm = T)
      sd[i] = mean(bad_breakout$sd_i, na.rm = T)
}

centers$los = los
#centers$range = range
#centers$sd = sd
View(centers)

#_______________Time Plots

time_plot = function(k){
      bad_time = filter(bad, cluster == k)
      len_stay = mean(as.numeric(bad_time$Length_Of_Stay_Seconds), na.rm = T)/3600/24
      sd_stay = sd(as.numeric(bad_time$Length_Of_Stay_Seconds), na.rm = T)/3600/24
      n_entries = nrow(bad_time)
      print(paste("patients in cluster = ", n_entries))
      print(paste('length of stay =', len_stay))
      print(paste('SD of Stay =', sd_stay))
      ids = select(bad_time, Visit_id)
      bad_breakout = filter(makeup, Visit_id %in% ids)%>%
            group_by(Visit_id)%>%
            summarise(entries = n())
      avg_entries = mean(bad_breakout$entries, na.rm = T)
      sd_entries = sd(bad_breakout$entries, na.rm = T)
      print(paste("average # of entries = ", avg_entries))
      print(paste("Standard Deviation of entries = ", sd_entries))
}

time_plot(1)
time_plot(2)
time_plot(3)
time_plot(4)
time_plot(5)


#__________

set.seed(1)
clusters_f_l = kmeans(bad[,c(2,3)], centers = 5, n = 100)

#take centers
centers = cbind(clusters_f_l$centers, count = clusters_f_l$size)
centers = data.frame(centers) #put centers into dataframe

# add centers to bad
bad$cluster = clusters_f_l$cluster

par(mfrow = c(1,1))
set.seed(1)
min_bad = jitter(as.numeric(bad$Min_GCS_Score), 2)
last_bad = jitter(as.numeric(bad$Last_GCS_Score),2)
plot(min_bad, last_bad, 
     xlab = "Minimum GCS Score", 
     ylab = "Last GCS Score", 
     col = bad$cluster, 
     main = "Min:Last Correlation Plot With Colored Clusters")
abline(h=12.4)








