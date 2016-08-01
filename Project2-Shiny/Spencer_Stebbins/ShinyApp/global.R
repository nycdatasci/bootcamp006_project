library(dplyr)
library(stringr)
# library(RCurl)

# v <- read.csv("../data/violence.csv")

# set source and destination port latitude and longitudes
#read in ihs data
ihs <- read.csv("./data/ihs.csv")
#fix date format func
ihs$Date <- as.Date(ihs$Date, format = "%m/%d/%Y")
ihs$C3 <- as.numeric(gsub(",", "", as.character(ihs$C3)))
ihs$C4 <- as.numeric(gsub(",", "", as.character(ihs$C4)))
#set volume based on C3 + C4
ihs$volume <- ihs$C3 + ihs$C4
#remove all rows where volume is 0
ihs <- ihs[as.numeric(ihs$volume) != 0, ]
# return((volume / max(volume, na.rm=TRUE)) * 4)
# Cut the volumes up into levels corresponding to the
# 75th, 50th, 25th, percentiles and then all the rest.
colors <- as.numeric(
  cut(ihs$volume,
      breaks=quantile(ihs$volume, probs=c(0,0.20,0.5,0.75,1), na.rm=TRUE),
      include.lowest=TRUE)
)
# Colors for each level
ihs$color <- c("#0055ff","#00aaff","#00ffaa","#aaff00")[colors]

# read in port data
# ports <-read.csv("../data/ports.csv")
# # portsDict <- list(ASSALUYEH = 'ASSULEYAH')
# # ports$PortName[df$Store.Type %in% c("A", "C")]
# 
# #substr from the end of string func
# substrRight <- function(x, n){
#   x <- as.character(x)
#   substr(x, nchar(x)-n+1, nchar(x))
# }
# #convert coordinates func
# convertCoords <- function (x) {
#   if (substrRight(x,3) == "' S" | substrRight(x,3) == "' W") {
#     x <- gsub(' ','.',substr(x,0,nchar(x)-3))
#     x <- as.numeric(x)
#     return(-x)
#   } else {
#     x <- gsub(' ','.',substr(x,0,nchar(x)-3))
#     return(as.numeric(x))
#   }
# }
# #convertlat and long columns
# ports$Latitude <- sapply(as.character(ports$Latitude), convertCoords)
# ports$Longitude <- sapply(as.character(ports$Longitude), convertCoords)
# #convert sources and destination to uppercase
# ihs$Source..2. <- toupper(ihs$Source..2.)
# ihs$Destination..2. <- toupper(ihs$Destination..2.)
# #join tables with new source and destination lat and longs
# ihs <- left_join(ihs, select(ports,PortName,Latitude,Longitude), by=c('Source..2.' ='PortName')) %>% 
#           rename(Source.Port.Lat=Latitude, Source.Port.Long=Longitude)
# ihs <- left_join(ihs, select(ports,PortName,Latitude,Longitude), by=c('Destination..1.' ='PortName')) %>%
#           rename(Destination.Port.Lat=Latitude, Destination.Port.Long=Longitude)
#set source and destination country latitude and longitudes
countries <- read.delim("./data/countries.csv")
#join tables with new source and destination lat and longs
ihs <- left_join(ihs, select(countries,Country,Latitude,Longitude), by=c('Source..1.' ='Country')) %>% 
  rename(Source.Country.Lat=Latitude, Source.Country.Long=Longitude)
ihs <- left_join(ihs, select(countries,Country,Latitude,Longitude), by=c('Primary.Destination.Country' ='Country')) %>%
  rename(Destination.Country.Lat=Latitude, Destination.Country.Long=Longitude)

# 
# inventories <- read.csv("../data/Propane Inventories.csv", header=FALSE)
# dates <- inventories[1, ]
# totals <- inventories[6, ]
# inventories <- data.frame()
# inventories <- rbind(dates[1, ])
# inventories[2,] <- rbind(totals[1,])
# inventories <- t(inventories)
# inventories <- inventories[-1,]
# x <- left_join(ihs, inventories, by=c("1","Date"))

# inventories <- read.csv("../data/Propane Inventories.csv")
# sources <- read.csv("../data/Volume by Source.csv")
# destinations <- read.csv("../data/Volume by Destination.csv")
# prices <- read.csv("../data/Spot Prices.csv")
# rates <- read.csv("../data/Freight Rates.csv")

# create variable with colnames as choice
themes <- c('Dark','Terrain')