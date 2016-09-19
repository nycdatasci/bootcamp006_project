library(dplyr)

ts_stats = ts_data%>%
      group_by(Visit_ID)%>%
      arrange(Time)%>%
      summarise(cluster = mean(Cluster),
                three = sum(GCS==3),
                four = sum(GCS==4),
                five = sum(GCS==5),
                six = sum(GCS==6),
                seven = sum(GCS==7),
                eight = sum(GCS==8),
                nine = sum(GCS==9),
                ten = sum(GCS==10),
                eleven = sum(GCS==11),
                twelve = sum(GCS==12),
                thirteen = sum(GCS==13),
                fourteen = sum(GCS==14),
                fifteen = sum(GCS==15),
                number_entries = n(),
                min_score = min(GCS),
                last_score = last(GCS),
                max_score = max(GCS),
                hours_min_score = sum(GCS==min(GCS)),
                avg_score = mean(GCS),
                time_below_mean = sum(GCS<avg_score),
                hours_u_6 = sum(GCS<6),
                hours_u_11 = sum(GCS<11),
                len_stay = n()/24,
                variety_of_scores = length(levels(as.factor(GCS)))
                )%>%
      mutate(outcome = ifelse(last_score>11, 1, 0))%>%
      mutate(four = three + four,
             five = four + five,
             six = five + six,
             seven = six + seven,
             eight = seven + eight,
             nine = eight + nine,
             ten = nine + ten,
             eleven = ten + eleven,
             twelve = eleven + twelve,
             thirteen = twelve + thirteen,
             fourteen = thirteen + fourteen,
             fifteen = fourteen + fifteen)

filtered = filter(ts_stats, ts_stats[,9]>=10)

save(ts_stats, file = 'ts_stats.RData')

three_five = filter(ts_stats, cluster %in% c(3,5))





