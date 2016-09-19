library(ggplot2)
library(ggmap)
library(dplyr)
library(reshape2)
library(zoo)
library(scales)
library(extrafont)
library(grid)
library(RPostgreSQL)
library(rgdal)
library(maptools)
library(raster)
library(imager)

uber_csv_names = c('apr14.csv','may14.csv','jun14.csv','jul14.csv','aug14.csv','sep14.csv')
uber = data.frame()
for (csv in uber_csv_names) {
  uber = rbind(uber, read.csv(paste('/Users/cholmes/Desktop/Capstone Project/Uber Data/uber-raw-data-', csv, sep ='')))
  print(csv)
}

##Seperating Data by Day of Week, Grouping by Lat/Lon rounded to 3 places, and by half an hour

#Assigning Days of Week and segmenting hours
uber$day = weekdays(as.Date(uber$Date.Time))
a <-as.POSIXlt(uber$Date.Time)
b <-as.POSIXlt(round(as.double(a)/(30*60))*(30*60),origin=(as.POSIXlt('1970-01-01')))
uber$rounded_hour = format(b,"%H:%M")

#Making new data frames for every day of week
days_of_week = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
for (weekday in days_of_week) {
  assign(paste('uber_',weekday, sep = ""), summarise(group_by(filter(uber, day == weekday), Lat, Lon, rounded_hour), count = n()))
}


## Most of this code is borrowed from https://github.com/toddwschneider/nyc-taxi-data/, thanks for the wonderful visualization, Todd!

# import spatial data for census tracts and neighborhoods
tracts = spTransform(readOGR("/Users/cholmes/Desktop/Capstone Project/nyct2010_15b", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))
tracts@data$id = as.character(as.numeric(rownames(tracts@data)) + 1)
tracts.points = fortify(tracts, region = "id")
tracts.map = inner_join(tracts.points, tracts@data, by = "id")

nyc_map = tracts.map
ex_staten_island_map = filter(tracts.map, BoroName != "Staten Island")
manhattan_map = filter(tracts.map, BoroName == "Manhattan")


alpha_range = c(0.14, 0.75)
size_range = c(0.134, 0.173)

time_of_day = c('00:00', '00:30','01:00', '01:30','02:00', '02:30','03:00', '03:30','04:00', '04:30','05:00', '05:30','06:00', '06:30','07:00', '07:30','08:00', '08:30', '09:00', '09:30','10:00', '10:30','11:00', '11:30','12:00', '12:30','13:00', '13:30','14:00', '14:30','15:00', '15:30','16:00', '16:30','17:00','17:30','18:00', '18:30','19:00', '19:30','20:00', '20:30','21:00', '21:30','22:00', '22:30','23:00', '23:30')
days_of_week = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

  for (time in time_of_day) {
    #Making a new dataframe based on day and time
    temp = filter(uber, rounded_hour == time)
    temp = summarise(group_by(temp, Lat, Lon), count = n())
    print(paste(time, week_day))
    p = ggplot() +
      geom_polygon(data = ex_staten_island_map,
                   aes(x = long, y = lat, group = group),
                   fill = "#080808", color = "#080808") +
      geom_point(data = temp,
                 aes(x = Lon, y = Lat, alpha = count, size = count, color = 'w')) +
      scale_alpha_continuous(range = alpha_range, trans = "log", limits = range(temp$count)) +
      scale_size_continuous(range = size_range, trans = "log", limits = range(temp$count)) +
      scale_color_manual(values = c("#ffffff", green_hex)) +
      coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
      theme(legend.position = "none") +
      ggtitle(paste('Uber Rides\n',time)) +
      theme_dark_map(base_size = 24) +
      theme(plot.title = element_text(size=150, face="bold", hjust = .0001, vjust = .8))
    
    fname = paste("/Users/cholmes/Uber-Maps/taxi_pickups_map", time, ".png")
    png(filename = fname, width =2950, height = 3795, bg = "black")
    print(p)
    dev.off()
  
  }
  





