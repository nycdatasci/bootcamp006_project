setwd('~/Desktop/US Domestic Flight shinyapp')
library(dplyr)
library(stringr)
flight <- read.csv('~/Desktop/FLIGHT/flight_data2.csv',stringsAsFactors=F)
# remove all the rows with flights = 0
flight<-flight[!flight$Flights == 0, ]
#remove all the rows with Passengers=0
flight<-flight[!flight$Passengers==0,]

#seperate Origin .City

flight.split <- str_split_fixed(flight$Origin.City, ", ", 2)
flight.split <- data.frame(flight.split,stringsAsFactors=FALSE)

#flight.split <- select(flight.split, State.abb=X2)

tbl_df(flight.split)

#merge two data frame to 
a<- cbind.data.frame(flight,O.state=flight.split$X2,stringsAsFactors=FALSE)
myflight <- select(a,-Fly.Date)

m <- str_split_fixed(myflight$Destination.City, ", ", 2)
m <- data.frame(m,stringsAsFactors=FALSE)

myflight <- cbind.data.frame(myflight,D.state=m$X2,stringsAsFactors=FALSE)

myflight <- myflight[!myflight$Seats==0,]

myflight <- myflight[!myflight$Distance==0,]

#write.csv(myflight,file = 'myflight.csv',row.names = F)



