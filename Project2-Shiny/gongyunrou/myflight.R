setwd('~/Desktop/gongyunrou.shinyapp/') 
library(dplyr)
library(ggplot2)
library(googleVis)
library(ggmap)
library(stringr)
library(DT)
myflight <- read.csv("myflight.csv",stringsAsFactors = F)

# Merge m - outflow; n- inflow,create the net inflow
i <- myflight%>%group_by(Year,Month,Origin,Origin.City,O.state)%>%
  summarise(Passengers=sum(Passengers),Flights=sum(Flights),
            Occupancy=mean(Occupancy),Population =mean(Origin.Population))
i <- ungroup(i)

o <- myflight%>%group_by(Year,Month,Destination,Destination.City)%>%
  summarise(Passengers=sum(Passengers),Flights=sum(Flights),
            Population =mean(Destination.Population))
o <- ungroup(o)

data <- merge(i,o,by.x = c("Origin","Year","Month"),by.y = c("Destination","Year","Month"))
  
data <- rename(data,Passengers_out=Passengers.x,Flights_out=Flights.x,Ori.Population=Population.x,
               Passengers_in=Passengers.y,Flights_in=Flights.y,Des.Population=Population.y )

data <- mutate(data, Netflow=Passengers_in-Passengers_out,
               Passengers=Passengers_in+Passengers_out,
               Flights=Flights_in+Flights_out)
data$Passengers_out <- NULL
data$Flights_out <- NULL
data$Passengers_in <- NULL
data$Flights_in <- NULL

##Annual data from each year 
annual<- data%>%
  group_by(Year)%>%
  summarise(TotalPassengers=sum(Passengers),TotalFlights=sum(Flights))

annual <- ungroup(annual)
# plot line chart
annual.plot<-gvisLineChart(
  data=annual,xvar = "Year",yvar=c("TotalFlights","TotalPassengers"),
  options=list(series="[{targetAxisIndex: 0},
               {targetAxisIndex:1}]",
               colors=c("#99ccff","orange"),
               width="1000px", height="500px",
               vAxes="[{title:'Number of Flights'},{title:'Number of Passengers'}]",
               hAxes="[{title:'Year'}]",
               title="Variation of Flights and Passengers from 1990 to 2009",
               titleTextStyle="{fontSize:18}",
               legend="{position:'Top'}"))
plot(annual.plot)

#filter to the primary airports
# data <- data%>%
#   filter(.,data$Origin %in% year$Origin)
yearlydata <- data%>%
  group_by(Year,Origin,Origin.City,O.state)%>%
  summarise(Passengers=sum(Passengers),Flights=sum(Flights),
            Occupancy=mean(Occupancy))

# ### primary airports, which handle more than 10,000 passengers each year
yearlydata <- yearlydata[yearlydata$Passengers>=10000,]
yearlydata<- ungroup(yearlydata)
# merge annual to get Total
yearlydata <- merge(yearlydata,annual,by="Year")
# add the airtype 
yearlydata <- yearlydata%>%
  mutate(AirportType=
           ifelse(Passengers<0.0005*TotalPassengers,"Nonhub",
                            ifelse(Passengers<0.0025*TotalPassengers,"SmallHub",
                                   ifelse(Passengers<0.01*TotalPassengers,"MediumHub",
                                          "LargeHub"))))

# year.Bubble <- gvisMotionChart(year,
#                               idvar="Origin", timevar ="Year",
#                               xvar="Occupancy", yvar="Flights",
#                               colorvar="Origin",
#                               options=list(width=850, height=650))
# plot(year.Bubble)

#merge by the year$ Airport Type 
airport <- select(yearlydata,Year,Origin,AirportType)

data<- merge(data,airport,by.x=c("Year","Origin"),by.y=c("Year","Origin"))

data <- rename(data,Airport=Origin,City=Origin.City,Population=Ori.Population)
data$Destination.City <- NULL
data$Des.Population <- NULL
# DC to MA state
data$O.state[data$O.state=='DC']<-'MA'


# change the O.state to full name of state
q <- data$O.state

#q <- str_trim(q,side='left')
y <- state.name[match(q,state.abb)]
data <- cbind(data,Ori.State=y)
data$O.state <- NULL
sum(is.na(data))

#rename to State
data <- rename(data,State=Ori.State)

data <- select(data,Airport,Year,Month, City,State,AirportType,everything())

#data <- data_location
# add location to data
USlocation <- read.csv("airport-codes.csv",stringsAsFactors = F)
USlocation <- subset(USlocation,USlocation$iso_country=="US")
USlocation <- select(USlocation,Name=name,latitude_deg,longitude_deg,Airport=local_code)
# missing from data
air <- unique(data$Airport)
air <- as.data.frame(air)
air <- mutate(air,include= air$air %in% USlocation$Airport)
# handling misiing , merge missing to USlocation
Name <- c("Glacier Park International Airport","Yuma MCAS/Yuma International Airport",
          "Sawyer International Airport","University Park Airport",
          "Phoenix-Mesa-Gateway Airport","Phoenix-Mesa-Gateway Airport","Branson Airport",
          "Chicago Meigs Airport"," Neets Bay Airport","Ketchikan Harbor Seaplane Base Airport",
          "Kenmore Air Harbor SPB Airport","Oak Harbor Airport")

latitude_deg <- c(48.3105011,32.656601,46.3536111,
                  40.8493004,33.3078003,33.3078003,
                  36.532082,41.858799,55.7789,
                  55.3498993,47.6288889,48.2833333
                  )
longitude_deg <- c(-114.2559967,-114.6060028,-87.3952778,
                   -77.8487015,-111.6549988,-111.6549988,
                   -93.200544,-87.6079025,-131.600998,
                   -131.677002,-122.3386111,-122.6166667
                   )
Airport <- c("FCA" ,"YUM" ,"MQT", "SCE", "DQF", "AZA" ,"BKG","CGX","DQU",
             "WFB","LKE","ODW")
miss <- cbind.data.frame(Name,latitude_deg,longitude_deg,Airport)

US <- bind_rows(USlocation,miss)

data <- merge(data,US,by="Airport")

sum(is.na(data))
# change the name of DQF to AZA

data$Airport[data$Airport=="DQF"] <- "AZA"

#write.csv(data,file="netflowdata.csv",row.names = F)

##plot the motion bubble graph eachyear in each month
data <- data%>%group_by(Airport,Year,Month,City,State,AirportType)%>%
  summarise(Flights=sum(Flights),Passengers=sum(Passengers),
            Population=mean(Population),Netflow=sum(Netflow),
            Occupancy=mean(Occupancy),latitude_deg=mean(latitude_deg),
            longitude_deg=mean(longitude_deg))

data$State <- as.character(data$State)

Bubble <- gvisMotionChart(filter(data,Year==1999), idvar="Airport", 
                          timevar = "Month",
                          xvar="Occupancy", yvar="Flights",
                          colorvar="AirportType", sizevar="Population",
                          options=list(width=850, height=650))
plot(Bubble)
### state map
data$State <- as.character(data$State)

M1<-filter(data,Year==1990 & Month==1)
M1<-M1%>%group_by(State)%>%
  summarise(Netflow=sum(Netflow))
#M1<-cbind.data.frame(M1,v)
#M1<-M1%>%
#group_by(State)%>%
#summarise(Passengers)
M1 <- as.data.frame(M1)

M7<-filter(data,Year==2009,Month==7)
M7<-M7%>%group_by(State)%>%
  summarise(Netflow=sum(Netflow))

M7 <- ungroup(M7)

#M7<-cbind.data.frame(M7,v)
# M7<-M7%>%
#   group_by(State)%>%
#   summarise(Passengers)



####Total Outflow of Passengers in JAN.2009 by State  

# plot map
geoflight<-gvisGeoChart(
  data%>%filter(.,Year==2009,Month==12)%>%
    group_by(State)%>%
    summarise(Netflow=sum(Netflow)),
  locationvar ="State",
  colorvar = "Netflow",
  options=list(region="US",
               dataMode="regions",
               resolution="provinces",
               colorAxis="{minValue:-450000,maxValue:450000,colors: ['#CC3300','#FFFFFF','green']}",
               width=800, height=600,
               title="Netflow of Passengers in JAN.1990 by State",
               titleTextStyle="{color:'#003366',fontName:'Arial',
               fontSize:18}",
               legend="{ position:'bottom'}"))
plot(geoflight)


### combine longtitude and latitude
mapAirport<- mutate(data,latlong = 
                      paste(latitude_deg,longitude_deg, 
                                    sep=":"))

#map with location 

mapAirport.plot <- gvisGeoChart(filter(mapAirport,Year==1990 & AirportType=="SmallHub"),
                                locationvar ="latlong",
                                colorvar = "Occupancy",
                                sizevar="Flights",
                                hovervar = "Airport",
                                #"Flights","Netflow","Airport",
                                options=list(displayMode="Markers", region="US",
                                colorAxis="{minValue:0,maxValue:1,colors:['#FAF2FC', '#9900CC']}",
                                backgroundColor="lightblue"))


plot(mapAirport.plot)


#Year comprison by each airport
year<- data%>%
  group_by(Airport,Year,City,AirportType)%>%
  summarise(Passengers=sum(Passengers),Flights=sum(Flights),
            Occupancy=mean(Occupancy),Population =mean(Population),
            Netflow=sum(Netflow),latitude_deg=mean(latitude_deg),
            longitude_deg=mean(longitude_deg))

year <- ungroup(year)
## Bubble plot year 
year.Bubble <- gvisMotionChart(year,
                              idvar="Airport", timevar ="Year",
                              xvar="Occupancy", yvar="Flights",
                              colorvar="Origin", sizevar="Population",
                              options=list(width=850, height=650))
plot(year.Bubble)

write.csv(data,file="data.csv",row.names = F)


te <- mapAirport%>%
  filter(Year==1990 & AirportType=="LargeHub")%>%
  group_by(Airport)%>%
  summarise(Flights=sum(Flights),
            Occupancy=mean(Occupancy),
            Population=round(mean(Population)))

