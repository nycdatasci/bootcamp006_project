library(dplyr)
library(tidyr)
library(RColorBrewer)
library(googleVis)
library(leaflet)

#load('crimedf1.RData')
load('crimedf1.RData')

# Extract days, months and years from date
crimedf1$Month_occ <- format(crimedf1$DATE.OCC1, '%b')
crimedf1$Year_occ <- format(crimedf1$DATE.OCC1, '%Y')
crimedf1$Day_occ <- format(crimedf1$DATE.OCC1, '%A')

# Show crime days using bar charts
daycount_df <- crimedf1 %>%
  group_by(Day_occ, Status.Desc) %>%
  summarise(volume = n()) %>% 
  spread(Status.Desc, volume)

daycount_df <- daycount_df[match(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), daycount_df$Day_occ),]
  

# Build a crime map of geographical distribution
crimedf2 <- crimedf1[!crimedf1$LOCATION == '', ]

locxy0 <- gsub('\\(|\\)', '', crimedf2$LOCATION) # reomve parentheses
locxy <- unlist(lapply(locxy0, strsplit, split = ', ')) # seperate x, y

lat_indexes<-seq(1,length(locxy),2) # select latitude data
long_indexes<-seq(2,length(locxy),2) # select longitude data
locx0 <- trimws(locxy[lat_indexes]) # remove whitespace
locy0 <- trimws(locxy[long_indexes]) # remove whitesapce

crimedf2$locx <- as.numeric(locx0)
crimedf2$locy <- as.numeric(locy0)
crimedf2 <- crimedf2[!(crimedf2$locx == 0 | crimedf2$locy == 0), ] # remove wrong locations (0, 0)

# Bar chart for crime areas
areaTOTcount_df <- crimedf1 %>%
  group_by(AREA.NAME) %>%
  summarise(volume = n())

areaTOTcount_df <- areaTOTcount_df[order(areaTOTcount_df$volume),]

# line chart of total cirmes changes
TOTcount_df <- crimedf1 %>%
  group_by(Year_occ) %>%
  summarise(volume = n())

# Calender
dycount_df <- crimedf1 %>% 
  group_by(DATE.OCC1) %>% 
  summarise(cnt = n())

# Crime Status
statcount_df <- crimedf1 %>%
  group_by(Status.Desc) %>%
  summarise(cnt = n(), perct = n()/nrow(crimedf1))

# Crimes data table 
dttable_df <- select(crimedf1, one_of(c('DATE.OCC1', 'Day_occ', 'TIME.OCC1', 'AREA.NAME', 'CrmCd.Desc', 'Status.Desc', 'Address', 'LOCATION')))

# Crimes time analysis


