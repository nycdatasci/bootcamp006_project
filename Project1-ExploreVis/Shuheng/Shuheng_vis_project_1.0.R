# NYC data science academy
# Project 1: Exploratory Visualization Project 
# Shuheng Li
# live long and prosper!

#########################################
#  Section 0 : Load Packages and Data   #
#########################################

#install.packages("ggthemes")
#install.packages('RgoogleMaps')
library(dplyr)
library(RgoogleMaps)
library(ggmap)
library(ggplot2)
library(ggthemes)

#getGeoCode("MANHATTAN")
#test API#

data <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv", stringsAsFactors = F) 
raw.df <- tbl_df(data)

#Load inspection data
#Rename and re-format columns

##################################
#  Section 1 : Data Processing   #
##################################

# convert data into better format for further analysis
names(raw.df) <- tolower(names(raw.df))
raw.df <- rename(raw.df, cuisine = cuisine.description) %>%
 mutate(inspection.date = as.Date(inspection.date, "%m/%d/%Y")) %>%
  mutate(grade.date = as.Date(grade.date, "%m/%d/%Y")) %>%
  mutate(record.date = as.Date(record.date, "%m/%d/%Y")) %>%
  mutate(phone = as.double(phone)) %>%
  mutate(latest_grade = ifelse(score < 0, 'Negative', 
                             ifelse(score < 13 , 'A',
                                    ifelse(score < 28, 'B', 'C')))) %>% 
  filter(boro != 'Missing' & score >= 0)


# add feature 'address' for getting Geocode
raw.df$address <- paste(raw.df$building, raw.df$street)

# get lastest grade
inspections <- unique(select(raw.df, camis, boro, zipcode, cuisine, 
                              inspection.date, action, score, latest_grade))
latest <- merge(aggregate(inspection.date ~ camis, inspections, max), inspections)

latest <- latest[complete.cases(latest),]

###########################
#  Section 2 : bar plot   #
###########################
# lastest grade barchart
ggplot(data=latest, aes(x= boro)) + 
  geom_bar(aes(fill=latest_grade), position='fill') +
  labs(title='Lastest Grade of Restaurants by Borough', 
       x='Borough', 
       y='Grade Portion') + 
  theme_bw() +
  theme_few() + scale_fill_few() 

###############################################
#  Section 3 : data preparation for mapping   #
###############################################

# fetching geocode, taking tons of time
location <- unique(manhattan$address)
location <- sapply(location, getGeoCode)
# write.csv(location, file = 'location_final.csv')

# load preserved Geocode data
location <- read.csv('location_final.csv')

# focus on Manhattan
manhattan <- raw.df[raw.df$boro == 'MANHATTAN',]
mapping <- inner_join(as.data.frame(manhattan), as.data.frame(location), by = 'address')

# calculate mean score
res_grade <- mapping %>% select(camis, score) %>% group_by(camis) %>% summarise(mean_score = mean(score))
res_grade <- mapping %>% select(camis, address, lon, lat) %>% group_by(address, lon, lat) %>% left_join(res_grade, by = 'camis')
res_grade <- unique(res_grade)
res_grade <- res_grade[complete.cases(res_grade),]

# getting NYC map focusing on Manhattan 
nycMapCoord <- c(-74.15,40.70, -73.77, 40.85) 
nycMap <- get_map(location = nycMapCoord, source="google", maptype="roadmap", color = 'bw', zoom = 13) 


#############################################
#  Section 4 : Average Grade Heatmap Plots  #
#############################################

#definite grade groups
grade_a <- subset(res_grade, res_grade$mean_score<13)
grade_b <- subset(res_grade, res_grade$mean_score>13 & res_grade$mean_score<28)
grade_c <- subset(res_grade, res_grade$mean_score>28)

#plot for A grade group
map_grade_a <- ggmap(nycMap) + 
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 2, bins = 4, data = grade_a, 
                 geom = 'polygon') + 
  scale_fill_gradient(low = "cyan", high = "darkblue", guide = FALSE) + 
  scale_alpha(range = c(0.4, 0.5), guide = FALSE) +
  labs(title='Heatmap for A (Average Grade) Restaurants in Manhattan', 
       x='Longitude', 
       y='Latitude')

#plot for B grade group
map_grade_b <- ggmap(nycMap) + 
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 2, bins = 4, data = grade_b, 
                 geom = 'polygon') + 
  scale_fill_gradient(low = "darkolivegreen1", high = "darkgreen", guide = FALSE) + 
  scale_alpha(range = c(0.4, 0.5), guide = FALSE) +
  labs(title='Heatmap for B (Average Grade) Restaurants in Manhattan', 
       x='Longitude', 
       y='Latitude')

#plot for C grade group
map_grade_c <- ggmap(nycMap) + 
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 2, bins = 4, data = grade_c, 
                 geom = 'polygon') + 
  scale_fill_gradient(low = "moccasin", high = "orange", guide = FALSE) + 
  scale_alpha(range = c(0.4, 0.5), guide = FALSE) +
  labs(title='Heatmap for C (Average Grade) Restaurants in Manhattan', 
       x='Longitude', 
       y='Latitude')

#################################
#  Avergae  Grade  ScatterPlot  #
#################################
# plot for whole dataet
map_grade <- ggmap(nycMap) +
  geom_point(aes(x = lon, y = lat), color = "blue", alpha = 0.4, size = 2, data = grade_a) +
  geom_point(aes(x = lon, y = lat), color = "green", alpha = 0.4, size = 2, data = grade_b) +
  geom_point(aes(x = lon, y = lat), color = "orange", alpha = 0.4, size = 2, data = grade_c) +
  labs(title='Scatterplot for Average Grade Restaurants in Manhattan', 
       x='Longitude', 
       y='Latitude')
  
# calculate compare feature 
res_grade_compare <- left_join(res_grade, latest, by = 'camis')
res_grade_compare <- mutate(res_grade_better, mean_grade = ifelse(mean_score < 0, 'Negative', 
                                                 ifelse(mean_score < 13 , 'A',
                                                 ifelse(mean_score < 28, 'B', 'C'))))

# divide data into three groups
res_grade_better <- subset(res_grade_compare, res_grade_compare$latest_grade > res_grade_compare$mean_grade)
res_grade_same <- subset(res_grade_compare, res_grade_compare$latest_grade == res_grade_compare$mean_grade)
res_grade_worse <- subset(res_grade_compare, res_grade_compare$latest_grade < res_grade_compare$mean_grade)

# plots for comparing
map_compare <- ggmap(nycMap) +
  geom_point(aes(x = lon, y = lat), color = "green", alpha = 0.7, size = 2, data = res_grade_better) +
  geom_point(aes(x = lon, y = lat), color = "grey20", alpha = 1.0, size = 2, shape= 1, data = res_grade_same) +
  geom_point(aes(x = lon, y = lat), color = "red", alpha = 0.4, size = 2, data = res_grade_worse) +
  labs(title='Scatterplot for Comparing Grades', 
       x='Longitude', 
       y='Latitude')
