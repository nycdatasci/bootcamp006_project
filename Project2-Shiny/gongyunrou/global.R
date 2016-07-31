library(googleVis)
library(dplyr)
library(stringr)
library(DT)
data <- read.csv("data.csv",stringsAsFactors = F)
# # convert matrix to dataframe
# state_stat <- data (state.name = rownames(state.x77), state.x77)
# # # remove row names
# rownames(state_stat) <- NULL
# # # create variable with colnames as choice
# choice <- colnames(state_stat)[-1]

# data for motion chart yearly 
year<- data%>%
  group_by(Airport,Year,City,AirportType)%>%
  summarise(Passengers=sum(Passengers),Flights=sum(Flights),
            Occupancy=mean(Occupancy),Population =mean(Population),
            Netflow=sum(Netflow),latitude_deg=mean(latitude_deg),
            longitude_deg=mean(longitude_deg))

year <- ungroup(year)
# data for motion chart monthly
month <- data%>%group_by(Airport,Year,Month,City,State,AirportType)%>%
  summarise(Flights=sum(Flights),Passengers=sum(Passengers),
            Population=mean(Population),Netflow=sum(Netflow),
            Occupancy=mean(Occupancy),latitude_deg=mean(latitude_deg),
            longitude_deg=mean(longitude_deg))

month$State <- as.character(month$State)

#data for map by state 



