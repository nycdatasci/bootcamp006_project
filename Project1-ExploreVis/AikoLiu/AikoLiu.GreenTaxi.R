library(dplyr)
library(lubridate)
library(reshape2)
library(stringr)
library(ggmap)
library(ggplot2)
library(ggrepel)
library(grid)
library(geojsonio)
library(sp)
library(rgdal)

## This is the R code for the green taxi portion of our visualization project

# Step 1. Data Pre-processing, data cleaning/filtering, etc.
# We bundle the 12 monthly NYC green taxi files into two half-year gz files by using cat, >> and gzip utilities.

# Loading the NYC geojson polygon file
nyc<-geojson_read('nycboroughboundaries.geojson',what='sp')
Boroughs = c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island') #BoroughCode Mapping

nyc2<-nyc
nyc2@data[,2:3]<-NULL #only keep the borough code, throw away the others

green_taxi1<-read.csv('green_tripdata_2015_01-06.csv.gz',stringsAsFactors = F)
green_taxi2<-read.csv('green_tripdata_2015_07-12.csv.gz',stringsAsFactors = F)

green_taxi1$dummy1 <- NULL  #handle the excess ',' in all the rows of the csv files
green_taxi1$dummy2 <- NULL

green_taxi2$dummy1 <- NULL
green_taxi2$dummy2 <- NULL

green_taxi<-rbind(green_taxi1,green_taxi2)

names(green_taxi) = c("VendorID"  ,            "Pickup_datetime"  ,"Dropoff_datetime" ,"Store_and_fwd_flag" ,  
                      "RateCodeID"        ,    "Pickup_longitude"  ,    "Pickup_latitude"   ,    "Dropoff_longitude" ,   
                      "Dropoff_latitude"   ,   "Passenger_count"    ,   "Trip_distance"     ,    "Fare_amount"  ,        
                      "Extra"       ,          "MTA_tax"       ,        "Tip_amount"        ,    "Tolls_amount" ,        
                      "Ehail_fee"    ,         "improvement_surcharge" ,"Total_amount"      ,    "Payment_type"  ,       
                      "Trip_type" )

rm(green_taxi1)
rm(green_taxi2)

PickupPts<-SpatialPoints(cbind(green_taxi$Pickup_longitude,green_taxi$Pickup_latitude))

PickupPts@proj4string <- nyc@proj4string
pickupBoroughCodes<-PickupPts %over% nyc2
rm(PickupPts)
green_taxi$boroughCode_p <- pickupBoroughCodes$boroughCode
rm(pickupBoroughCodes)

DropoffPts<-SpatialPoints(cbind(green_taxi$Dropoff_longitude,green_taxi$Dropoff_latitude))
DropoffPts@proj4string <- nyc@proj4string
dropoffBoroughCodes<-DropoffPts %over% nyc2
rm(DropoffPts)
green_taxi$boroughCode_d <- dropoffBoroughCodes$boroughCode
rm(dropoffBoroughCodes)

green_taxi$month <- month(green_taxi$Pickup_datetime)
green_taxi$wday  <- wday(green_taxi$Pickup_datetime)
green_taxi$hour  <- hour(green_taxi$Pickup_datetime)

# Change the format of datetime from string to POSIXct objects

green_taxi$Pickup_datetime <- as.POSIXct(green_taxi$Pickup_datetime,format='%Y-%m-%d %H:%M:%S')
green_taxi$Dropoff_datetime <- as.POSIXct(green_taxi$Dropoff_datetime,format='%Y-%m-%d %H:%M:%S')

green_taxi$duration <- floor(as.double(green_taxi$Dropoff_datetime-green_taxi$Pickup_datetime)/60.0)
green_taxi$percent  <- green_taxi$Tip_amount/green_taxi$Fare_amount * 100.0
green_taxi$speed    <- green_taxi$Trip_distance/green_taxi$duration *60

save(green_taxi,file='green_tripdata_2015.RData')  # saving the raw data to R native binary format for 
# future usage

# Even though the data has no missing values (it has some unused column whose data is missing), the 
# data is very dirty. Source: untrained human input error, GPS instrument error...,etc. We have to apply
# filter to remove those outliers which can damage our analysis. Furthermore, we need to fiter away
#taxi rides which are actually FHV (for hire vehicle) rides which can last several hours and a few hundred
#dollors.  We fiter these rare events away.

green_adm <- filter(green_taxi, RateCodeID %in% c(1,2,6), Payment_type %in% 1:2)
# filter away non-credit card/cash ride and rides to Westchester county,etc.
rm(green_taxi)
# From now on, green_adm will be our data

#1. Time Domain Analysis

# data cleaning

green_clean<-filter(green_adm,Fare_amount<200,Total_amount<1000,Tolls_amount<30,Fare_amount>0,Trip_distance<50,
                    Trip_distance>0.02,duration<120,duration>=1,speed>0,speed<100,percent<100,
                    !is.na(boroughCode_p),!is.na(boroughCode_d))

nrow(green_clean) # after partial cleaning, we have > 18M green-taxi data for 2015
rm(green_adm)

by_passenger<-green_clean %>% group_by(passenger=factor(Passenger_count)) %>% summarise(count=n())
ggplot(data=by_passenger,aes(x=passenger,y=count))+geom_bar(
stat = 'identity',aes(fill=passenger)) +scale_y_continuous(name='taxi ride count',breaks=seq(0,1.5e7,by=5e6),
labels=c('0M','5M','10M','15M'))+xlab('Passenger Count') + ggtitle('green taxi passenger count statistics')+theme(legend.position='none')

by_payment <- green_clean %>% group_by(Payment_type) %>% summarise(percent=mean(percent),count=n())
ggplot(data=by_payment,aes(x=as.factor(Payment_type),y=count,fill=percent))+geom_bar(stat = 'identity') +ylab('taxi ride count') + scale_x_discrete(
  name='Payment type', labels=c('Credit Card','Cash'))

by_dist <- green_clean %>% filter(Trip_distance<10) %>% group_by(dist=floor(2*Trip_distance)*0.5) %>% summarise(count=n(),speed=mean(speed))
ggplot(data=by_dist,aes(x=as.factor(dist),y=count,fill=speed))+geom_bar(stat = 'identity') +ylab('taxi ride count')+scale_x_discrete(
name='Travel Distance',breaks=seq(0,10,by=1))

by_speed <- green_clean %>% filter(speed<50) %>% group_by(speed=floor(2*speed)*0.5) %>% summarise(count=n(),percent_sd=sd(percent), duration=mean(duration))
ggplot(data=by_speed,aes(x=as.factor(speed),y=count))+geom_bar(stat = 'identity',aes(fill=I('green'))) +ylab('taxi ride count')+scale_x_discrete(
  name='avg speed (mph)',breaks=seq(0,50,by=10)) + ggtitle('green taxi ride counts vs speed')

by_duration <- green_clean %>% filter(duration<60) %>% group_by(duration=floor(2*duration)*0.5) %>% summarise(count=n(),speed=mean(speed))
ggplot(data=by_duration,aes(x=as.factor(duration),y=count,fill=speed))+geom_bar(stat = 'identity') +ylab('taxi ride count')+scale_x_discrete(
  name='duration of the ride',breaks=seq(0,60,by=5))

by_month <-green_clean %>% group_by(month=factor(month)) %>% summarise(.,speed=mean(speed),duration=mean(duration),
                                                   dist=mean(Trip_distance),percent=mean(percent),count=n())

ggplot(data=by_month,aes(x=month,y=count,fill=speed))+geom_bar(stat = 'identity') +scale_y_continuous(name='taxi ride count',breaks=seq(0,1.5e6,by=5e5),
labels=c('0M','0.5M','1M','1.5M'))+ggtitle('monthly green ride counts colored by the speeds')+scale_fill_gradient(low='darkgreen',high='white',name='speed (mph)')
                                                                                                                                                                          
ggplot(data=by_month,aes(x=as.factor(month),y=dist,fill=duration))+geom_bar(stat = 'identity') +ylab('taxi ride avg distance')

# We investigate which month has the higher traffic jam rate
jammed <- filter(green_clean,speed<5,speed>0.5) %>% group_by(month) %>% summarise(count=n(),speed=mean(speed))
background <- filter(green_clean,speed>0.5) %>% group_by(month=factor(month)) %>% summarise(count=n())
jammed$ratio <- jammed$count/background$count*100
ggplot(data=jammed,aes(x=as.factor(month),y=ratio,fill=speed))+geom_bar(stat = 'identity') +ylab(
  'jam percentage')+scale_x_discrete(name='month') + ggtitle("Jammed rides percentages colored by speed")




# next we move to the wday-hour analysis

by_wday <-green_clean %>% group_by(.,wday=factor(wday)) %>% summarise(.,speed=mean(speed),duration=mean(duration),
                                                          dist=mean(Trip_distance),percent=mean(percent),count=n()/365.0)


ggplot(data=by_wday,aes(x=wday,y=count,fill=speed))+geom_bar(stat = 'identity') +ylab('taxi ride count/day') + scale_x_discrete(
  name='week day', labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S')) + scale_fill_gradient(low='darkgreen',high='white',name='speed (mph)')

# Surprisingly, Sunday has the most taxi rides, Monday the next, the tues-Friday the rides increase
# monotonically, but the speed is slow.

ggplot(data=by_wday,aes(x=as.factor(wday),y=duration,fill=dist))+geom_bar(stat = 'identity') +ylab('taxi ride duration') + scale_x_discrete(
  name='week day', labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S'))

ggplot(data=by_wday,aes(x=as.factor(wday),y=speed,fill=duration))+geom_bar(stat = 'identity') +ylab('taxi speed') + scale_x_discrete(
  name='week day', labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S'))

by_wdayNpayment <-green_clean %>% group_by(.,wday,Payment_type=factor(Payment_type,levels=1:2,labels=c('card payment','cash payment'))) %>% summarise(.,speed=mean(speed),duration=mean(duration),
dist=mean(Trip_distance),percent=mean(percent),count=n()/365.0)


g<-ggplot(data=by_wdayNpayment,aes(x=as.factor(wday),y=count,fill=Payment_type))
g<-g+geom_bar(stat = 'identity',position='dodge',show.legend=T)
g<-g+ylab('taxi ride count/day') + scale_x_discrete(name='week day', labels=c('1'='S','2'='M','3'='T','4'='W',
                            '5'='T','6'='F','7'='S'))
g

# We study the weekday with the highest travel jam percentage

jammed <- filter(green_clean,speed<5,speed>0.5) %>% group_by(wday) %>% summarise(count=n(),speed=mean(speed))
background <- filter(green_clean,speed>0.5) %>% group_by(wday) %>% summarise(count=n())
jammed$ratio <- jammed$count/background$count*100
ggplot(data=jammed,aes(x=factor(wday),y=ratio,fill=speed))+geom_bar(stat = 'identity') +ylab('jam percentage')+scale_x_discrete(
  name='week day', labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S'))


# next we display the by hour data distribution

by_hour<-green_clean %>% group_by(.,hour) %>% summarise(.,speed=mean(speed),duration=mean(duration),
                                                        dist=mean(Trip_distance),percent=mean(percent),count=n()/365.0) 

ggplot(data=by_hour,aes(x=as.factor(hour),y=count,fill=speed))+geom_bar(stat = 'identity') +ylab('taxi ride avg count/day') + scale_x_discrete(
  name='hours')

ggplot(data=by_hour,aes(x=as.factor(hour),y=speed,fill=dist))+geom_bar(stat = 'identity') +ylab('taxi ride speed') + scale_x_discrete(
  name='hours')

ggplot(data=by_hour,aes(x=as.factor(hour),y=duration,fill=speed))+geom_bar(stat = 'identity') +ylab('taxi ride duration') + scale_x_discrete(
  name='hours')

# We study during the 24 hours, which hour has the higher traffic jam rate: < 5 mph

jammed <- filter(green_clean,speed<5,speed>0.5) %>% group_by(hour) %>% summarise(count=n(),speed=mean(speed))
background <- filter(green_clean,speed>0.5) %>% group_by(hour) %>% summarise(count=n())
jammed$ratio <- jammed$count/background$count*100

ggplot(data=jammed,aes(x=hour,y=ratio,fill=speed))+geom_bar(stat = 'identity') +ylab('jam percentage')

# next we display the joint data by wday and hour using 2D heat map in ggplot

by_wdayNhour<-green_clean %>% group_by(.,wday,hour) %>% summarise(.,speed=mean(speed),duration=mean(duration),
Payment_type=mean(Payment_type), dist=mean(Trip_distance),percent=mean(percent),count=n()/365.0) 

ggplot(by_wdayNhour, aes(x=as.factor(wday), y=as.factor(hour),fill=count)) + geom_tile() + scale_fill_gradientn(colors=c('black','dark red','red',
'orange','yellow','white')) + scale_x_discrete(name='week day', 
labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S'))+ggtitle(
'The number of rides in a weekday vs hour heat map') + ylab('hour') + labs(fill="Daily Ride Count")

ggplot(by_wdayNhour, aes(x=as.factor(wday), y=as.factor(hour),fill=speed)) + geom_tile() + scale_fill_gradientn(colors=c('black','dark red','red',
'orange','yellow','white')) + scale_x_discrete(name='week day', 
labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S'))+ggtitle(
'The green taxi speed in a weekday vs hour heat map') + ylab('hour') + labs(fill="Avg Speed (mph)")

ggplot(by_wdayNhour, aes(x=as.factor(wday), y=as.factor(hour),fill=duration)) + geom_tile() + scale_fill_gradientn(colors=c('black','dark red','red',
'orange','yellow','white')) + scale_x_discrete(name='week day', 
labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S'))+ggtitle(
'the trip duration in a weekday vs hour heat map') + ylab('hour') + labs(fill="duration (min)")

ggplot(by_wdayNhour, aes(x=as.factor(wday), y=as.factor(hour),fill=dist)) + geom_tile() + scale_fill_gradientn(colors=c('black','dark red','red',
'orange','yellow','white')) + scale_x_discrete(name='week day', 
labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S'))+ggtitle(
'the trip distance in a weekday vs hour heat map') + ylab('hour') + labs(fill="distance (miles)")

ggplot(by_wdayNhour, aes(x=as.factor(wday), y=as.factor(hour),fill=Payment_type-1.0)) + geom_tile() + scale_fill_gradientn(colors=c('black','dark red','red',
'orange','yellow','white'),breaks=c(0.5,0.6),labels=c('50% cash payment','60% cash payment')) + scale_x_discrete(name='week day', 
labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S'))+ggtitle(
'The cash vs card payment in a weekday vs hour heat map') + ylab('hour') + labs(fill="Payment Type")


# Because the cash tip is mostly not reported, not in the TLC system, we filter out the cash payment
# to estimate the tip-percentages.

by_wdayNhour_card<-green_clean %>% filter(Payment_type==1) %>% group_by(.,wday,hour) %>% summarise(.,speed=mean(speed),duration=mean(duration),
                                      dist=mean(Trip_distance),percent=mean(percent),count=n()/365.0) 


ggplot(by_wdayNhour_card, aes(x=as.factor(wday), y=as.factor(hour),fill=percent)) + geom_tile() + scale_fill_gradientn(colors=c('black','dark red','red',
'orange','yellow','white')) + scale_x_discrete(name='week day', 
labels=c('1'='S','2'='M','3'='T','4'='W','5'='T','6'='F','7'='S'))+ggtitle(
'The Tip percentage in a weekday vs hour heat map') + ylab('hour') + labs(fill="tip percentage")

# From the heat maps, we notice that the tip percentages somehow are correlated to the speed of the taxi.
# We want to investigate their relationship

by_speed<-green_clean %>% filter(Payment_type==1,speed<50,percent<50) %>% group_by(speed=floor(2*speed)/2) %>% summarise(
duration=mean(duration),percent=mean(percent),dist=mean(Trip_distance),count=n()/365.0)

ggplot(by_speed, aes(x=as.factor(speed),y=count),fill='green') + geom_bar(stat='identity',color='green') + scale_x_discrete(name='speed (mph)',breaks=seq(0,50,by=5))


ggplot(by_speed, aes(x=(speed), y=percent)) + geom_point(aes(color
=count),size=2,shape=1) + scale_x_continuous(name='speed (mph)',breaks=seq(0,50,by=5))+geom_smooth(se=F) + ylab('tip percentage')

# We refine this into different hours in the day

newLevel<-c(rep('0-6am',6),rep('6-9am',3),rep('9am-4pm',7),rep('4-7pm',3),rep('7-12pm',5))

by_speed_H <-green_clean %>% filter(Payment_type==1,speed<30,percent<50) %>% group_by(speed=floor(2*speed)/2,hour=factor(hour)) %>% summarise(
  duration=mean(duration),percent=mean(percent),dist=mean(Trip_distance),count=n()/365.0)

levels(by_speed_H$hour)<-newLevel

g<-ggplot(by_speed_H, aes(x=(speed), y=percent))  + scale_x_continuous(
name='speed (mph)',breaks=seq(0,50,by=10))+ geom_smooth(se=F,aes(color=I('green'))) + ylab('tip percentages')
g+facet_wrap(~hour) + ggtitle('The Tip Percentages at Differerent Speeds')

# duration plot

ggplot(by_speed, aes(x=(speed), y=duration)) + geom_point(aes(color
=count),size=2,shape=1) + scale_x_continuous(name='speed (mph)',breaks=seq(0,50,by=5))+geom_smooth(se=F) + ylab('duration (minutes)')

g<-ggplot(by_speed_H, aes(x=(speed), y=duration)) + scale_x_continuous(
  name='speed (mph)',breaks=seq(0,50,by=10))+geom_smooth(se=F,aes(color=I('green'))) + ylab('duration (minutes)')
g+facet_wrap(~hour) + ggtitle('The Ride Duration at Differerent Speeds')

ggplot(by_speed, aes(x=(speed), y=dist)) + geom_point(aes(color
=count),size=2,shape=1) + scale_x_continuous(name='speed (mph)',breaks=seq(0,50,by=5))+geom_smooth(se=F)+ylab('trip distance (miles)')

g<-ggplot(by_speed_H, aes(x=(speed), y=dist)) + scale_x_continuous(
  name='speed',breaks=seq(0,50,by=10))+geom_smooth(se=F,aes(color=I('green'))) + ylab('Trip distance (miles)')
g+facet_wrap(~hour) + ggtitle('The Ride Distance at Differerent Speeds')

#  We point out that the drop of tip percentage is in part due to the change of the proportion of
# passengers who decide not to pay the tips w.r.t. the speed of the taxi.

noPay_byspeed<-green_clean %>% filter(Payment_type==1,speed<50,percent==0.0) %>% group_by(speed=floor(2*speed)/2) %>% summarise(
  duration=mean(duration),percent=mean(percent),dist=mean(Trip_distance),count=n()/365.0)

noPay_byspeed$ratio = noPay_byspeed$count/by_speed$count * 100

ggplot(noPay_byspeed, aes(x=(speed), y=ratio)) + geom_point(size=2,shape=1) + scale_x_continuous(name='speed (mph)',breaks=seq(0,50,by=5))+geom_smooth(se=F)+ylab('non-tippers\' population percentages')

pay_byspeed<-green_clean %>% filter(Payment_type==1,speed<50,percent>0,percent<50) %>% group_by(speed=floor(2*speed)/2) %>% summarise(
  duration=mean(duration),percent=mean(percent),dist=mean(Trip_distance),count=n()/365.0)

ggplot(pay_byspeed, aes(x=(speed), y=percent)) + geom_point(size=2,shape=1) + scale_x_continuous(name='speed (mph)',breaks=seq(0,50,by=5))+geom_smooth(se=F)+ylab('Tip percentages for those who pay')

pay_byspeed_H <- green_clean %>% filter(Payment_type==1,speed<50,percent>0,percent<50) %>% group_by(speed=floor(2*speed)/2,hour=factor(hour)) %>% summarise(
  duration=mean(duration),percent=mean(percent),dist=mean(Trip_distance),count=n()/365.0)

x<-pay_byspeed_H[,c('speed','hour','percent','duration','dist')]
levels(x) <-newLevel

g<-ggplot(x, aes(x=(speed), y=percent))  + scale_x_continuous(
  name='speed (mph)',breaks=seq(0,30,by=10))+geom_smooth(se=F) + ylab('tip percentage')
g+facet_wrap(~hour) + ggtitle("Tip percentages for those who pay")

g<-ggplot(x, aes(x=(speed), y=duration))  + scale_x_continuous(
  name='speed (mph)',breaks=seq(0,30,by=10))+geom_smooth(se=F) + ylab('duration (minutes)')
g+facet_wrap(~hour) + ggtitle("Trip duration for those who pay")

g<-ggplot(x, aes(x=(speed), y=dist))  + scale_x_continuous(
  name='speed (mph)',breaks=seq(0,30,by=10))+geom_smooth(se=F) + ylab('dist (miles)')
g+facet_wrap(~hour) + ggtitle("Trip distance for those who pay")

noPay_byspeed_H <- green_clean %>% filter(Payment_type==1,speed<50,percent == 0) %>% group_by(speed=floor(2*speed)/2,hour=factor(hour)) %>% summarise(
  duration=mean(duration),percent=mean(percent),dist=mean(Trip_distance),count=n()/365.0)



# geo-spatial analysis

stat_p<- green_clean %>% group_by(Pickup_Borough=boroughCode_p) %>% summarise(count=n())
stat_d<- green_clean %>% group_by(Destination_Borough=boroughCode_d) %>% summarise(count=n())

stat_p$Pickup_Borough      <- factor(Boroughs)
stat_d$Destination_Borough <- factor(Boroughs)

g<-ggplot(data=stat_p,aes(x=Pickup_Borough,y=count,color=Pickup_Borough)) + geom_bar(
  stat='identity',aes(fill=Pickup_Borough))
g + ylab('green taxi rides in 2015') + scale_x_discrete(name='5 NYC Boroughs',
labels=c('Brnx','Brklyn','Man','Qns','Staten'))+scale_y_continuous(breaks=seq(0,6e6,by=2e6),labels=c('0M','2M','4M','6M'))

g<-ggplot(data=stat_d,aes(x=Destination_Borough,y=count,color=Destination_Borough)) + geom_bar(
  stat='identity',aes(fill=Destination_Borough)) + ylab('green taxi rides in 2015')
g + scale_x_discrete(name='5 NYC Boroughs', labels=c('Bronx','Brklyn','Manhattan','Queens','Staten')) +scale_y_continuous(breaks=seq(0,6e6,by=2e6),labels=c('0M','2M','4M','6M'))


connection<- green_clean %>% filter(speed>1,speed<80,percent<50,duration<60,Trip_distance<20) %>% 
  group_by(Pickup_Borough=boroughCode_p,Destination_Borough=boroughCode_d) %>%
  summarise(count=n(),dist=mean(Trip_distance), speed=mean(speed),duration=mean(duration),percent=mean(percent))

connection <- filter(connection,Pickup_Borough!=5,Destination_Borough!=5)

connection$Pickup_Borough <- factor(connection$Pickup_Borough,labels=Boroughs[1:4])
connection$Destination_Borough <- factor(connection$Destination_Borough,labels=Boroughs[1:4])


# starting boroughs vs target borough in a bar graph 

ggplot(data=connection,aes(x=Pickup_Borough,y=count)) + geom_bar(stat='identity',aes(fill=Destination_Borough),
position='dodge') + xlab('Starting Borough') + ylab('green taxi rides in 2015') + scale_y_continuous(breaks=seq(0,4e6,by=2e6),
labels=c('0M','2M','4M'))



ggplot(connection, aes(x=as.factor(Pickup_Borough), y=as.factor(Destination_Borough),fill=speed)) + geom_tile() + scale_fill_gradientn(colors=c('black','dark red','red',
    'orange','yellow','white')) + scale_x_discrete(name='Green Taxi Originated Borough') +ggtitle(
'speed-origin vs target borough heat map') + ylab('Target Borough') + labs(fill="speed")


by_origin_hour <- green_clean %>% filter(speed>1,speed<80,percent<50,duration<60,Trip_distance<20,boroughCode_p!=5) %>% 
  group_by(Pickup_Borough=factor(boroughCode_p,labels=Boroughs[1:4]),hour=factor(hour)) %>%
  summarise(count=n(),dist=mean(Trip_distance), speed=mean(speed),duration=mean(duration),percent=mean(percent))


#by_origin_hour$Pickup_Borough <- factor(rep(Boroughs[1:4],each=12))

ggplot(data=by_origin_hour,aes(x=as.factor(Pickup_Borough),y=as.factor(hour))) + geom_tile(aes(fill=count)) + scale_fill_gradientn(colors=c('black','dark red','red',
'orange','yellow','white'), breaks=seq(1e5,4e5,by=1e5),labels=c('0.1M','0.2M','0.3M','0.4M')) + scale_x_discrete(name='Originated Borough') +ggtitle(
'The ride count in an origin vs hour heat map') + ylab('hour') + labs(fill="daily taxi rides")


ggplot(data=by_origin_hour,aes(x=Pickup_Borough,y=as.factor(hour))) + geom_tile(aes(fill= speed)) + scale_fill_gradientn(colors=c('black','dark red','red',
 'orange','yellow','white')) + scale_x_discrete(name='Originated Borough') +ggtitle(
 'The speed in an origin vs hour heat map') + ylab('hour') + labs(fill="speed")

#### by_origin_wday
 
by_origin_wday<- green_clean %>% filter(speed>1,speed<80,percent<50,duration<60,Trip_distance<20,boroughCode_p!=5) %>%
   group_by(Pickup_Borough=factor(boroughCode_p,labels=Boroughs[1:4]),wday) %>% 
   summarise(count=n()/365,dist=mean(Trip_distance), speed=mean(speed),duration=mean(duration),percent=mean(percent))
 
 ggplot(data=by_origin_wday,aes(x=Pickup_Borough,y=factor(-wday))) + geom_tile(
   aes(fill= count)) + scale_fill_gradientn(colors=c('black','dark red','red',
'orange','yellow','white')) + scale_x_discrete(name='Originated Borough')+scale_y_discrete(
name='week day',breaks=-1:-7,labels=c('S','M','T','W','T','F','S')
) +ggtitle('The daily taxi rides in an origin vs week day heat map') + labs(fill="daily taxi ride")
 
 
 
############## Google map section

nyc_map_g_str <- get_map(location = "new york city", zoom = 11)

green_partial<- transmute(green_adm,lat_p=Pickup_latitude,lon_p=Pickup_longitude,
lat_d=Dropoff_latitude,lon_d=Dropoff_longitude,month,wday,hour,duration,percent,
Payment_type,speed,Trip_type,Passenger_count,boroughCode_p,boroughCode_d)

green_clean<-filter(green_partial,lon_d > -80,lon_d< -65,lat_d>39,lat_d<46,lon_p > -80,
                   lon_p< -65,lat_p>39,lat_p<46)

green_clean <- mutate(green_clean, lon_p=signif(lon_p,5),lat_p=signif(lat_p,5),lon_d=signif(lon_d,5),lat_d=signif(lat_d,5))

nrow(clean_clean) # 18.6M rows

green_sample <- sample_frac(green_clean, size=0.1) # sample to reduce the sample size ~ 2M

PickUpPlot<-function(green_sample,title='') {
ggmap(nyc_map_g_str, extent = "device") + stat_density2d(data = green_sample, aes(x = lon_p, y = lat_p, fill = ..level.., alpha = ..level..), size = 0.01, bins = 80, geom = "polygon") + scale_fill_gradient(low = "blue", high
  = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE) + theme(legend.position='none') + ggtitle(title)
}

DropOffPlot<-function(green_sample,title=""){
ggmap(nyc_map_g_str, extent = "device") + stat_density2d(data = green_sample, aes(x = lon_d, y = lat_d, fill = ..level.., alpha = ..level..), size = 0.01, bins = 80, geom = "polygon") + scale_fill_gradient(low = "blue", high
= "red") + scale_alpha(range = c(0, 0.3), guide = FALSE) + theme(legend.position='none')+ggtitle(title)
}

#The general rule is to ask the question, filtering green_clean, saving the result to green_sample.
#Use row(green_sample) to check if the number of rows if >2M. If it is too large, stat_density2d
#is not powerful enough to deal with such large data set. Trim the data by random sampling using 
# sample_frac to reduce the sample size to ~ 2M rows or less.  Then input into PickupPlot and DropOffPlot
# to overlay on the google map.

# Because the process is very similar to each other. We only list the filtering process, not the identical display
# function calls.


# where is the extra slow traffic 

green_sample <- filter(green_clean,speed>0.5,speed<5)

nrow(green_sample)



# morning rush hours
green_sample<-filter(green_clean,hour %in% 6:8)

# afternoon rush hours
green_sample<-filter(green_clean,hour %in% 16:19)

nrow(green_sample)

green_sample<-sample_frac(green_sample,size=0.4)


PickUpPlot(green_sample)

DropOffPlot(green_sample)

green_sample<-filter(green_clean,duration>10)
green_sample<-sample_frac(green_sample,size=0.20)

# Are there different patterns for cash or card passengers?

green_sample<-filter(green_clean,Payment_type==1)


green_sample<-filter(green_clean,Payment_type==2)

# conclusion, the people who pay cash do not go to lower Manhattan by taxi.
# Instead, those who take taxis to lower Manhattan tend to pay credit cards!  It is related to credit worthiness.

# high speed taxi-rides
green_sample<-filter(green_clean, speed>20)
green_sample<-sample_frac(green_sample,size=0.7)

# 
green_sample <- filter(green_clean, wday %in% 2:5)
green_sample<-sample_frac(green_sample,size=0.2)

green_sample <- filter(green_clean, wday %in% c(1,6:7))
green_sample<-sample_frac(green_sample,size=0.2)

green_sample <- filter(green_clean, Trip_type==2)   # more wide spread, even though the total 
#number of such rides 400K+ has been filtered down to 7K by our filtering process.

green_sample <- filter(green_clean, speed>50) # about 94K, mostly from north Manhattan (Harem)

green_sample <- filter(green_clean, speed>20,speed<50) # About 2.2M rides, mostly going to La guadia
# a lot of them are from Harem

green_sample <- filter(green_clean, speed>13, speed<20)

green_sample <- filter(green_clean, speed>20,speed<50,Payment_type==1)

green_sample <- filter(green_clean, percent>30)

green_sample <- filter(green_clean, speed < 13)

green_sample <- filter(green_clean, boroughCode_p==1)



