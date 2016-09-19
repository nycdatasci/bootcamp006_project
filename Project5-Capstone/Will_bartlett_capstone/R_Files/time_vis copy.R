library(psych)
library(dplyr)
library(ggplot2)

setwd("~/Desktop/Data_Science/GCS_Folder/Rdata_Files")

load('makeup.RData')
load('patients.RData')
load('bad.RData')
load('makeup_clusters.RData')

#_______get min time________

min_score_time = makeup_clusters %>%
      group_by(Visit_id) %>%
      filter(`Glasgow Coma Scale Score`==min(`Glasgow Coma Scale Score`))%>%
      filter(Recorded_time==min(Recorded_time))
      
#___________plot all patients of a vector of clusters__________
plot_cluster = function(clus_vect, min_score_t = TRUE){
      options(expressions=10000)
      set.seed(3)
      bad_x = filter(bad, cluster %in% clus_vect)
      unique = bad_x$Visit_id
      g = ggplot() + 
            #scale_x_continuous(limits = c(0, 100)) + 
            scale_color_discrete() + 
            ylab("Score Entry #") +
            xlab("GCS Score (jittered)")
            ggtitle(paste("GCS Score Progression for each Patient in Clusters ", clus_vect))
      for(i in 1:length(unique)){
            df = filter(makeup_clusters, makeup_clusters$Visit_id == unique[i])
            df = arrange(df, Recorded_time)
            min_s_time = filter(min_score_time, Visit_id == unique[i])
            min_s_time = min_s_time$Recorded_time[1]
            if(min_score_t == TRUE){df = filter(df, Recorded_time >= min_s_time)}
            df$index = c(1:nrow(df))
            df_ends = df[c(1,nrow(df)),]
            g = g + geom_line(data = df, 
                              aes(x = index, 
                                  y = jitter(`Glasgow Coma Scale Score`), 
                                  color = as.factor(cluster),
                                  alpha = .1)) +
                  geom_point(data = df_ends, 
                             aes(x = index,
                                 y = jitter(`Glasgow Coma Scale Score`),
                                 size = 3))
      }
      g
}

plot_cluster(c(5), min_score_t = TRUE)

#___________create index DF___________

load('makeup_clusters.RData')

index = data.frame()
unique = bad$Visit_id
for(i in 1:length(unique)){
df = filter(makeup_clusters, makeup_clusters$Visit_id == unique[i])
df = arrange(df, Recorded_time)
min_s_time = filter(min_score_time, Visit_id == unique[i])
min_s_time = min_s_time$Recorded_time[1]
df = filter(df, Recorded_time >= min_s_time)
df$index = c(1:nrow(df))
indexed = rbind(index, df)
print(paste("Number of Patients Indexed = ", i))
print(paste("Last Row of Index =", indexed[nrow(indexed),]))
}

load('indexed.RData')
View(indexed)

#________________time plots for clusters_______________

#___create split index dfs_______
index_clust_1 = indexed%>%
      filter(cluster==1)%>%
      group_by(index)%>%
      summarise(count = n(),
                avg_GCS_score = mean(`Glasgow Coma Scale Score`),
                sd_GCS_score = sd(`Glasgow Coma Scale Score`),
                two_sd_below = avg_GCS_score - 2*(sd_GCS_score),
                two_sd_above = avg_GCS_score + 2*(sd_GCS_score),
                avg_EYE_score = mean(`Eye Opening`),
                avg_MOTOR_score = mean(`Best Motor Response`),
                avg_VERBAL_score = mean(`Best Verbal Response`))%>%
      filter(count>25)%>%
      mutate(cluster = as.factor(1))%>%
      mutate(rounded_avg = round(avg_GCS_score))%>%
      mutate(rounded_avg_eye = round(avg_EYE_score))%>%
      mutate(rounded_avg_motor = round(avg_MOTOR_score))%>%
      mutate(rounded_avg_verbal = round(avg_VERBAL_score))
      
index_clust_2 = indexed%>%
      filter(cluster==2)%>%
      group_by(index)%>%
      summarise(count = n(),
                avg_GCS_score = mean(`Glasgow Coma Scale Score`),
                sd_GCS_score = sd(`Glasgow Coma Scale Score`),
                two_sd_below = avg_GCS_score - 2*(sd_GCS_score),
                two_sd_above = avg_GCS_score + 2*(sd_GCS_score),
                avg_EYE_score = mean(`Eye Opening`),
                avg_MOTOR_score = mean(`Best Motor Response`),
                avg_VERBAL_score = mean(`Best Verbal Response`))%>%
      filter(count>25)%>%
      mutate(cluster = as.factor(2))%>%
      mutate(rounded_avg = round(avg_GCS_score))%>%
      mutate(rounded_avg_eye = round(avg_EYE_score))%>%
      mutate(rounded_avg_motor = round(avg_MOTOR_score))%>%
      mutate(rounded_avg_verbal = round(avg_VERBAL_score))

index_clust_3 = indexed%>%
      filter(cluster==3)%>%
      group_by(index)%>%
      summarise(count = n(),
                avg_GCS_score = mean(`Glasgow Coma Scale Score`),
                sd_GCS_score = sd(`Glasgow Coma Scale Score`),
                two_sd_below = avg_GCS_score - 2*(sd_GCS_score),
                two_sd_above = avg_GCS_score + 2*(sd_GCS_score),
                avg_EYE_score = mean(`Eye Opening`),
                avg_MOTOR_score = mean(`Best Motor Response`),
                avg_VERBAL_score = mean(`Best Verbal Response`))%>%
      filter(count>25)%>%
      mutate(cluster = as.factor(3))%>%
      mutate(rounded_avg = round(avg_GCS_score))%>%
      mutate(rounded_avg_eye = round(avg_EYE_score))%>%
      mutate(rounded_avg_motor = round(avg_MOTOR_score))%>%
      mutate(rounded_avg_verbal = round(avg_VERBAL_score))

index_clust_4 = indexed%>%
      filter(cluster==4)%>%
      group_by(index)%>%
      summarise(count = n(),
                avg_GCS_score = mean(`Glasgow Coma Scale Score`),
                sd_GCS_score = sd(`Glasgow Coma Scale Score`),
                two_sd_below = avg_GCS_score - 2*(sd_GCS_score),
                two_sd_above = avg_GCS_score + 2*(sd_GCS_score),
                avg_EYE_score = mean(`Eye Opening`),
                avg_MOTOR_score = mean(`Best Motor Response`),
                avg_VERBAL_score = mean(`Best Verbal Response`))%>%
      filter(count>25)%>%
      mutate(cluster = as.factor(4))%>%
      mutate(rounded_avg = round(avg_GCS_score))%>%
      mutate(rounded_avg_eye = round(avg_EYE_score))%>%
      mutate(rounded_avg_motor = round(avg_MOTOR_score))%>%
      mutate(rounded_avg_verbal = round(avg_VERBAL_score))

index_clust_5 = indexed%>%
      filter(cluster==5)%>%
      group_by(index)%>%
      summarise(count = n(),
                avg_GCS_score = mean(`Glasgow Coma Scale Score`),
                sd_GCS_score = sd(`Glasgow Coma Scale Score`),
                two_sd_below = avg_GCS_score - 2*(sd_GCS_score),
                two_sd_above = avg_GCS_score + 2*(sd_GCS_score),
                avg_EYE_score = mean(`Eye Opening`),
                avg_MOTOR_score = mean(`Best Motor Response`),
                avg_VERBAL_score = mean(`Best Verbal Response`))%>%
      filter(count>25)%>%
      mutate(cluster = as.factor(5))%>%
      mutate(rounded_avg = round(avg_GCS_score))%>%
      mutate(rounded_avg_eye = round(avg_EYE_score))%>%
      mutate(rounded_avg_motor = round(avg_MOTOR_score))%>%
      mutate(rounded_avg_verbal = round(avg_VERBAL_score))

save.image(file = 'Indexed_clusters.RData')
#______________avg time activity for each cluster plots_________________
#_______all entries (with 25 or more patients)
g = ggplot(data = index_clust_1, aes(x = index, y = avg_GCS_score, color = cluster)) + 
      geom_line() +
      scale_x_continuous(limits = c(0, 666)) + 
      scale_color_discrete() +
      ggtitle("Avg GCS Score for Entries after Minimum Score for Each Cluster") + 
      ylab("Avg GCS Score") + 
      xlab("Entry #") + scale_y_continuous(breaks = c(1:15))
g = g + geom_line(data = index_clust_2, aes(x = index, y = avg_GCS_score, color = cluster))
g = g + geom_line(data = index_clust_3, aes(x = index, y = avg_GCS_score, color = cluster))
g = g + geom_line(data = index_clust_4, aes(x = index, y = avg_GCS_score, color = cluster))
g = g + geom_line(data = index_clust_5, aes(x = index, y = avg_GCS_score, color = cluster))

g

#_______First 100 entries after min score_______

g_100 = ggplot(data = index_clust_1, aes(x = index, y = avg_GCS_score, color = cluster)) + 
      geom_line() +
      scale_x_continuous(limits = c(0, 100)) + 
      scale_color_discrete() + 
      ggtitle("Avg GCS Score for Entries after Minimum Score for Each Cluster") + 
      ylab("Avg GCS Score") + 
      xlab("Entry #") + scale_y_continuous(breaks = c(1:15))
g_100 = g_100 + geom_line(data = index_clust_2, aes(x = index, y = avg_GCS_score, color = cluster))
g_100 = g_100 + geom_line(data = index_clust_3, aes(x = index, y = avg_GCS_score, color = cluster))
g_100 = g_100 + geom_line(data = index_clust_4, aes(x = index, y = avg_GCS_score, color = cluster)) 
g_100 = g_100 + geom_line(data = index_clust_5, aes(x = index, y = avg_GCS_score, color = cluster)) 

g_100


#__________rounded, 100__________

g_100 = ggplot(data = index_clust_1, aes(x = index, y = rounded_avg, color = cluster)) + 
      geom_line() +
      scale_x_continuous(limits = c(0, 100)) + 
      scale_color_discrete() + scale_y_continuous(breaks = c(1:15)) +
      ggtitle("Avg (rounded) GCS Score for Entries after Minimum Score for each cluster") + 
      ylab("Avg GCS Score (rounded)") + 
      xlab("Entry #")
g_100 = g_100 + geom_line(data = index_clust_2, aes(x = index, y = rounded_avg, color = cluster))
g_100 = g_100 + geom_line(data = index_clust_3, aes(x = index, y = rounded_avg, color = cluster))
g_100 = g_100 + geom_line(data = index_clust_4, aes(x = index, y = rounded_avg, color = cluster))
g_100 = g_100 + geom_line(data = index_clust_5, aes(x = index, y = rounded_avg, color = cluster))

g_100


three_scores = function(x, nindex = NULL, ncount = NULL){
      setwd("~/Desktop/Data_Science/GCS_Folder/Rdata_Files")
      load('Indexed_clusters.RData')
      load("centers.RData")
      if(x == 1){df = index_clust_1
      center = centers[1,]}
      if(x == 2){df = index_clust_2
      center = centers[2,]}
      if(x == 3){df = index_clust_3
      center = centers[3,]}
      if(x == 4){df = index_clust_4
      center = centers[4,]}
      if(x == 5){df = index_clust_5
      center = centers[5,]}
      if(is.null(nindex)==F){df = filter(df, index<nindex)}
      if(is.null(ncount)==F){df = filter(df, count>ncount)}
      plot = ggplot() + 
            geom_line(data = df, aes(x = index, 
                                     y = avg_GCS_score,
                                     color = "Total",
                                     label = "Total")) +
            geom_line(data = df, aes(x = index,
                                     y = avg_EYE_score, 
                                     color = "Eye",
                                     label = "Eye")) +
            geom_line(data = df, aes(x = index, 
                                     y = avg_VERBAL_score, 
                                     color = "Verbal",
                                     label = "Verbal")) +
            geom_line(data = df, aes(x = index, 
                                     y = avg_MOTOR_score, 
                                     color = "Motor",
                                     label = "Motor")) +
            scale_y_continuous(breaks = c(1:15), limits = c(1,15)) +
            ggtitle(paste("Cluster #", x, 
                          ", Center =", 
                          round(center$Min_GCS_Score,2), 
                          ",", 
                          round(center$Last_GCS_Score,2))) +
            ylab("Average GCS Score") +
            xlab("Entry #")
      plot
}

three_scores(1)

grid

