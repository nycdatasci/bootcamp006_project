#library needed in cleaning data
#install.packages('mapproj')
#install.packages('leaflet')
#install.packages('sp')
#install.packages('ggmap')
#install.packages('dygraphs')
library(ggmap)
library(mapproj)
library(dplyr)
library(leaflet)
library(sp)
library(dygraphs)

#read data
setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Shiny/Realty_price/Shiny_App')
mydata = read.csv("F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Shiny/Realty_price/Shiny_App/data/Condo_price.csv", header = TRUE)
mydata = mydata[1:4209,]
head(mydata)
names(mydata)
mydata$ADDRESS = paste0(mydata$ADDRESS,', New York')

#find geolocation
#lo = geocode(as.character(mydata$ADDRESS[1:10]))
#long_lat = geocode(as.character(mydata$ADDRESS[11:1500]))
#long_lat2 = geocode(as.character(mydata$ADDRESS[1501:2490]))
#long_lat3 = geocode(as.character(mydata$ADDRESS[2491:2500]))
#long_lat4 = geocode(as.character(mydata$ADDRESS[2501:4209]))
#write.csv(lo,'lo.csv')
#write.csv(long_lat,'long_lat.csv')
#write.csv(long_lat2,'long_lat2.csv')
#write.csv(long_lat3,'long_lat3.csv')
#write.csv(long_lat4,'long_lat4.csv')

lo = read.csv('./data/lo.csv')
long_lat = read.csv('./data/long_lat.csv')
long_lat2 = read.csv('./data/long_lat2.csv')
long_lat3 = read.csv('./data/long_lat3.csv')
long_lat4 = read.csv('./data/long_lat4.csv')
geoloc = rbind(lo,long_lat,long_lat2,long_lat3,long_lat4)
#write.csv(geoloc,'geoloc.csv')
#geoloc = read.csv('geoloc.csv')
#create clean data set
#geoloc = append(long_lat,long_lat2)
#df1 = read.csv('lon1.csv')

x=c()
for (i in 1:4209){
  if((geoloc$lon[i]<(-74.042015))|(geoloc$lon[i]>(-73.830528))|(geoloc$lat[i]<40.650283)|(geoloc$lat[i]>40.896760)){
    x=rbind(x,i)
  }
}

#convert currency to numeric
y <- sub('\\$','',as.character(mydata$PRICE.FT2))
cost.FT2 <- as.numeric(sub('\\,','',as.character(y)))

dollarToNumber_vectorised <- function(vector) {
  
  # Want the vector as character rather than factor while
  # we're doing text processing operations
  vector <- as.character(vector)
  vector <- gsub("(\\$|,)","", vector)
  # Create a numeric vector to store the results in, this will give you
  # warning messages about NA values being introduced because the " K" values
  # can't be converted directly to numeric
  result <- as.numeric(vector)
  # Find all the "$N K" values, and modify the result at those positions
  k_positions <- grep("K", vector)
  result[k_positions] <- as.numeric(gsub("K","", vector[k_positions])) * 1000
  # Same for the "$ M" value
  m_positions <- grep("M", vector)
  result[m_positions] <- as.numeric(gsub("M","", vector[m_positions])) * 1000000
  return(result)
}
cost = dollarToNumber_vectorised(mydata$PRICE)

#Clean data
newdata = data.frame(mydata,cost,cost.FT2,geoloc)
finaldata=newdata[-x,]
#onebed = newdata[newdata$BEDS=='1 bd',]

#get the map
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-73.99014, lat=40.76436, popup="This is where you get started")
m

leaflet(geoloc[-x,]) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)