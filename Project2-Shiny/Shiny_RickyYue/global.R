library(dplyr)
library(rgdal)

## read entropy table
df = read.csv("C:/Users/ricky/Dropbox/bootcamp/project 2/result.csv")
## read world map
countries <- readOGR("C:/Users/ricky/Dropbox/bootcamp/project 2/ne_10m_admin_0_map_units","ne_10m_admin_0_map_units", verbose = FALSE)
## select interested regions
league_countries <- subset(countries, countries@data$NAME %in% c("France","Italy","Portugal","Netherlands","Spain","Germany","England","Scotland"))
## add an new column to the map table with country names
league_countries@data$ID <- c("Germany","England","Spain","France","Italy","Netherlands","Portugal","Scotland")    
df_sea <- df %>% group_by(season,country)%>%summarize(entropy=mean(entropy))
df_sea$country <- as.character(df_sea$country) 
df_sea$season <- as.character(df_sea$season)