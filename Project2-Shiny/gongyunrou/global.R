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
##Annual data from each year 
annual<- data%>%
  group_by(Year)%>%
  summarise(TotalPassengers=sum(Passengers),TotalFlights=sum(Flights))
annual <- ungroup(annual)

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

#data for Airport location map
mapAirport<- mutate(year,latlong = 
                      paste(latitude_deg,longitude_deg, 
                            sep=":"))

mapAirport <- mapAirport%>%
  group_by(Year,Airport,AirportType,latlong)%>%
  summarise(Flights=sum(Flights),Passengers=sum(Passengers),
            Population=mean(Population),Netflow=sum(Netflow),
            Occupancy=mean(Occupancy))

mapAirport <- ungroup(mapAirport)

# choice <- c("SmallHub",  "Nonhub","MediumHub", "LargeHub")


