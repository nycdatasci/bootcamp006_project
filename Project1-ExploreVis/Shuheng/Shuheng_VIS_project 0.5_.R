install.packages('RgoogleMaps')
library(dplyr)
library(RgoogleMaps)
library(ggmap)
library(ggplot2)
getGeoCode("MANHATTAN")
#test R package#

data <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv", stringsAsFactors = F) 
raw.df <- tbl_df(data)

#Load inspection data
#Rename and re-format columns

#names(raw.df) <- tolower(names(raw.df))
#raw.df <- rename(raw.df, cuisine = cuisine.description) %>%
#  mutate(., inspection.date = as.Date(inspection.date, "%m/%d/%Y")) %>%
#  mutate(., grade.date = as.Date(grade.date, "%m/%d/%Y")) %>%
#  mutate(., record.date = as.Date(record.date, "%m/%d/%Y")) %>%
#  mutate(., phone = as.double(phone)) 

raw.df$address <- paste(raw.df$building, raw.df$street)
#class(raw.df$address)
#address <- as.matrix(raw.df$address)
manhattan <- raw.df[raw.df$boro == 'MANHATTAN',]


location <- unique(manhattan$address)
location <- sapply(location, getGeoCode)


mapping <- inner_join(as.data.frame(manhattan), as.data.frame(location), by = 'address')

res_grade <- mapping %>% select(dba, score) %>% group_by(dba) %>% summarise(mean_score = mean(score))
res_grade <- res_grade[complete.cases(res_grade),]

res_grade <- mapping %>% select(dba, address, lon, lat) %>% group_by(address, lon, lat) %>% right_join(res_grade, by = 'dba')
res_grade <- unique(res_grade)
res_grade <- res_grade[complete.cases(res_grade),]

nycMapCoord <- c(-74.15,40.70, -73.77, 40.85) 
nycMap <- get_map(location = nycMapCoord, source="google", maptype="roadmap", color = 'bw', zoom = 13) 


res_map <- ggmap(nycMap) + 
  geom_point(aes(x = lon, y = lat), color = "red", alpha = 0.4, size = 2, data = subset(res_grade, res_grade$mean_score<14)) + 
  geom_point(aes(x = lon, y = lat), color = "blue", alpha = 0.4, size = 2, data = subset(res_grade, res_grade$mean_score>14 & res_grade$mean_score<28)) + 
  geom_point(aes(x = lon, y = lat), color = "green", alpha = 0.4, size = 2, data = subset(res_grade, res_grade$mean_score>28)) 
  
res_map

#res_grade <- filter(res_grade, res_grade$lat < 40.85 & res_grade$lat > 40.70 & res_grade$lon < -73.77 & res_grade$lon > -74.15)
