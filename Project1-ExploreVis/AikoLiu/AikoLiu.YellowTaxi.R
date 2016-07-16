library(dplyr)
library(lubridate)
library(reshape2)
library(stringr)
library(ggmap)
library(ggplot2)
library(ggrepel)
library(grid)
library(saves)
library(geojsonio)
library(sp)
library(rgdal)


# The yellow taxi data is parallel to the green taxi data. But the total size of 2015 files is more than
# 10 times the size of the green taxi.  The size > 20G is larger than the memory of our local laptop.
# Thus it cannot be loaded into the R/RStudio naively. On the other hand, the data visualization 
# process is more or less parallel to the green taxi data. So we focus on how to process the big data effectively
# within the capability of R/RStudio, by introducing the steps similar to the Hadoop map-reduce operation.
# 
# We also make use the 'saves' package to break the monthly data frame of the csv data into many 1 column data frame,
# stored in the native R binary format RData and archive into a TAR file called xxx.RDatas
# In this way, we can load (through random access) the needed columns without going through the total data and
#  reparse it each time.   We dump the statistics per file cumulatively into the yellow_taxi_dump.R, which is human 
# readable

nyc<-geojson_read('nycboroughboundaries.geojson',what='sp')  # load the nyc spatial polygon data frame
Boroughs = c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Stanten Island') #BoroughCode Mapping

months = str_pad(1:12,2,"left",pad='0')
yellow_taxi_files         = paste0('yellow_tripdata_2015-',months,'.csv.gz')
yellow_taxi_output_Files  = paste0('yellow_adm_2015_',months,'.RDatas')

varNames = c()  # will store the header names of the variables in yellow_taxi_dump.R

# The bottleneck of our computation is the RAM ~ 16G. Otherwise, we could have used
#the multi-threading support of R (FOREACH) to parallelly compute the monthly map and reduce steps
#as they are totally un-related.

for (month in 1:12) {
  
  yellow_adm <- Preprocessing.Yellow.Taxi(yellow_taxi_files[month],nyc)
  # 'saves' stores the yellow_adm as many 1 column data.frames 
  saves(yellow_adm,file=yellow_taxi_output_Files[month],overwrite=T)
  varNames <- Map.PartialReduce.yellow.taxi(yellow_adm)
  rm(yellow_adm)
  gc()
}

###############
# After restarting a new session (or rm(list=ls()) ), assuming that the relavent data has been saved into the files
library(dplyr)
library(lubridate)
library(reshape2)
library(stringr)
library(saves)
library(ggplot2)
library(ggmap)

source('yellow_taxi_dump.R')  # source and dump to the file, dump allows writing in an appending mode
# varNames are from the previous session
by_wday <- Assign.yellow.taxi('by_wday',months=1:3) # aggregating the monthly tables into a single data frame by aggregating
by_wday <- Averaging.yellow.taxi(by_wday) #conver the sum statistics to mean statistics



# source will populate the variables from 'yellow_taxi_dump.R' into the global environment
# the variables are named by x01, x02, .... where x in varNames and 01, 02, ... 12 are the month codes

# We illustrate the procedure to cumulative the 12 xxx.RDatas files into a single data frame,
# only getting some specific columns, down-sampling to cut down the sample size, in order to 
# be used by ggplot, ggmap

nyc_map_g_str <- get_map(location = "new york city", zoom = 11)  # google map of the NYC 

months = str_pad(1:12,2,"left",pad='0')


yellow_files = paste0('yellow_adm_2015_',months,'.RDatas')
colNames = c('Pickup_longitude','Pickup_latitude','percent','boroughCode_p','boroughCode_d')
inNYC    = list()
for (i in 1:12) {
yellow_partial<-loads(file=yellow_files[i],variables=colNames, to.data.frame=T)
# test the boroughCodes to determine if the origin and the destination are in the city
inNYC[[i]] <- (!is.na(yellow_partial$boroughCode_p))&(!is.na(yellow_partial$boroughCode_d))
rm(yellow_partial)
if (i%%3==0) {gc()}
}

colNames <- c('Pickup_longitude','Pickup_latitude','duration','percent')

yellow_samples = as.data.frame(matrix(nrow=0,ncol=length(colNames)))
colnames(yellow_samples) <- colNames

for (i in 1:12) {
  
  yellow_partial<-loads(file=yellow_files[i],variables=colNames, to.data.frame=T)

  yellow_samples <- rbind(yellow_samples, sample_frac(filter(yellow_partial,inNYC[[i]]),size=0.015))
  rm(yellow_partial)  # tell the garbage collector that it can discard the variable
  if (i%%3==0) { gc() }  # calling garbage collector
}


yellow_sample <- transmute(yellow_samples, lon=signif(Pickup_longitude,5), lat=signif(Pickup_latitude,5))

#or 

yellow_sample <- transmute(yellow_samples, lon=signif(Dropoff_longitude,5), lat=signif(Dropoff_latitude,5))

rm(yellow_samples)

yellow_sample<-filter(yellow_sample,lon > -80,lon< -65,lat > 39,lat< 46)
# rounding the long, lat to the desired accuracies

ggmap(nyc_map_g_str, extent = "device") + stat_density2d(data = yellow_sample, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 80, geom = "polygon") + scale_fill_gradient(low = "blue",
high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)+ theme(legend.position='none') +ggtitle('yellow taxi origination plot')



rm(yellow_sample)


####################################################################################
# The followings are the functions used in the above codes

# This is a map step mapping the yellow_taxi raw data to the format we desire, yellow_adm

Preprocessing.Yellow.Taxi<-function(fileName,nyc) {

library(saves)
library(stringr)
library(sp)
  
Boroughs = c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Stanten Island') #BoroughCode Mapping

nyc2<-nyc
nyc2@data[,2:3]<-NULL

yellow_taxi<-read.csv(fileName,stringsAsFactors = F)
 
print(paste("Finish reading the yellow taxi file for month",month))
names(yellow_taxi)=c("VendorID","Pickup_datetime","Dropoff_datetime",
                     "Passenger_count","Trip_distance","Pickup_longitude","Pickup_latitude","RateCodeID","Store_and_fwd_flag",
                     "Dropoff_longitude","Dropoff_latitude","Payment_type","Fare_amount","Extra",
                     "Mta_tax","Tip_amount","Tolls_amount","Improvement_surcharge","Total_amount")

# Convert the long, lat info to the NYC borough categories
PickupPts<-SpatialPoints(cbind(yellow_taxi$Pickup_longitude,yellow_taxi$Pickup_latitude))

PickupPts@proj4string <- nyc@proj4string
pickupBoroughCodes<-PickupPts %over% nyc2
rm(PickupPts)
yellow_taxi$boroughCode_p <- pickupBoroughCodes$boroughCode
rm(pickupBoroughCodes)

DropoffPts<-SpatialPoints(cbind(yellow_taxi$Dropoff_longitude,yellow_taxi$Dropoff_latitude))
DropoffPts@proj4string <- nyc@proj4string
dropoffBoroughCodes<-DropoffPts %over% nyc2
rm(DropoffPts)
yellow_taxi$boroughCode_d <- dropoffBoroughCodes$boroughCode
rm(dropoffBoroughCodes)

# convert the datetime string into month, wday, hour, etc.

yellow_taxi$month = month(yellow_taxi$Pickup_datetime)
yellow_taxi$wday  = wday(yellow_taxi$Pickup_datetime)
yellow_taxi$hour  = hour(yellow_taxi$Pickup_datetime)

# add derived variables which we will analyze

yellow_taxi$duration = floor(as.double(as.POSIXct(yellow_taxi$Dropoff_datetime,format='%Y-%m-%d %H:%M:%S')
                                       -as.POSIXct(yellow_taxi$Pickup_datetime,format='%Y-%m-%d %H:%M:%S'))/60.0)

yellow_taxi$speed   = (yellow_taxi$Trip_distance/yellow_taxi$duration)*60
yellow_taxi$percent = (yellow_taxi$Tip_amount/yellow_taxi$Fare_amount) *100
yellow_taxi$Pickup_datetime<-NULL
yellow_taxi$Dropoff_datetime<-NULL

# Restrict to the credit card/cash payment, standard fare, JFK rides and group rides, etc.
# we do not intend to include negotiated fares or payment by non-standard method.

yellow_adm <- filter(yellow_taxi,RateCodeID %in% c(1,2,6),Payment_type %in% 1:2)
print(paste('month', month, 'raw number of rows:', nrow(yellow_taxi)))
rm(yellow_taxi)
return(yellow_adm)
}

# This is a map and (partially) reduce step which cleans the yellow_adm, filtering, grouping them into 
# different small tables with the 'sum' of the various variables (like speed, duration ... etc) as the columns, which is saved into the human readable yellow_taxi_dump.R 

Map.PartialReduce.yellow.taxi<-function(yellow_adm) {

  yellow_clean<-yellow_adm %>% filter(Fare_amount<200,Total_amount<1000,Tolls_amount<30,Fare_amount>0,Trip_distance<50,
                       Trip_distance>0.02,duration<120,duration>=1,speed>0,speed<100,percent<100)
  print(paste('month', month, 'yellow_clean number of rows:',nrow(yellow_clean)))
  
  x<-select(yellow_clean,month,wday,hour,duration,speed, percent,Tip_amount,Fare_amount,Total_amount,Tolls_amount,
            Trip_distance,Payment_type,RateCodeID,Passenger_count,boroughCode_p,boroughCode_d)
  rm(yellow_clean)
  
  y<-transmute(x,Passenger_count,month,wday,hour,duration,percent, Fare_amount, speed, Total_amount, Tolls_amount, Trip_distance,Payment_type,RateCodeID,boroughCode_p,boroughCode_d)
  y<-filter(y,speed<80,percent<50,duration<60,Trip_distance<20)
  y<-filter(y,Fare_amount<200,Total_amount<1000,Tolls_amount<30,Fare_amount>0,Trip_distance<20,
            Trip_distance>0.02,duration<120,duration>=1,speed>0,
            !is.na(boroughCode_p),!is.na(boroughCode_d))
  
  by_passenger  <- y %>% group_by(Passenger_count) %>% summarise(count=n())
  by_wday_hour  <- y %>% group_by(wday,hour) %>% summarise(percentSum=sum(percent), durationSum=sum(duration), speedSum=sum(speed), distSum=sum(Trip_distance), count=n())
  by_month      <- y %>% group_by(month) %>% summarise(percentSum=sum(percent), durationSum=sum(duration), speedSum=sum(speed), distSum=sum(Trip_distance), count=n())
  by_wday       <- y %>% group_by(wday) %>% summarise(percentSum=sum(percent), durationSum=sum(duration), speedSum=sum(speed), distSum=sum(Trip_distance), count=n())
  by_hour       <- y %>% group_by(hour) %>% summarise(percentSum=sum(percent), durationSum=sum(duration), speedSum=sum(speed), distSum=sum(Trip_distance), count=n())
  by_origin_hour<- y %>% group_by(Pickup_Borough=factor(boroughCode_p,labels=Boroughs),hour=factor(hour)) %>% 
    summarise(percentSum=sum(percent), durationSum=sum(duration), speedSum=sum(speed), distSum=sum(Trip_distance), count=n())
  by_speed      <- y %>% filter(Payment_type==1,speed<50) %>% group_by(speed=floor(speed*2)*0.5) %>% summarise(percentSum=sum(percent), durationSum=sum(duration), distSum=sum(Trip_distance), count=n())
  by_speed_hour <- y %>% filter(Payment_type==1,speed<50) %>% group_by(speed=floor(speed*2)*0.5,hour) %>% summarise(percentSum=sum(percent), durationSum=sum(duration), distSum=sum(Trip_distance), count=n())
  by_duration   <- y %>% filter(duration<60) %>% group_by(duration=floor(2*duration)*0.5) %>% summarise(percentSum=sum(percent),speedSum=sum(speed),distSum=sum(Trip_distance), count=n())
  by_dist       <- y %>% filter(Trip_distance<10) %>% group_by(dist=floor(2*Trip_distance)*0.5) %>% summarise(percentSum=sum(percent),speedSum=sum(speed),durationSum=sum(duration), count=n())
  connection<- y %>% group_by(Pickup_Borough=factor(boroughCode_p,labels=Boroughs),
                              Destination_Borough=factor(boroughCode_d,labels=Boroughs)) %>%
    summarise(percentSum=sum(percent), durationSum=sum(duration), speedSum=sum(speed), distSum=sum(Trip_distance), count=n())
  pay_byspeed_H <- y %>% filter(Payment_type==1,speed<50,percent>0,percent<50) %>% group_by(speed=floor(2*speed)/2,hour=factor(hour)) %>% summarise(
    duration=mean(duration),percent=mean(percent),dist=mean(Trip_distance),count=n()/365.0)
  noPay_byspeed_H <- y %>% filter(Payment_type==1,speed<50,percent==0) %>% group_by(speed=floor(2*speed)/2,hour=factor(hour)) %>% summarise(
    duration=mean(duration),dist=mean(Trip_distance),count=n()/365.0)
  
  newLevel<-c(rep('0-6am',6),rep('6-9am',3),rep('9am-4pm',7),rep('4-7pm',3),rep('7-12pm',5))
  
  myList <- c('noTipG_by_speed','tipG_by_speed', 'by_passenger', 'by_wday_hour','by_speed', 'pay_byspeed_H','noPay_byspeed_H',
              'by_month','by_wday', 'by_hour', 'by_speed_hour','by_origin_hour', 'by_dist', 'by_duration','connection')
  
  
  noTipG  <-filter(y,percent==0)
  noTipG_by_speed <- noTipG %>% group_by(floor(speed*2)*0.5) %>% summarise(speedSum=sum(speed),count=n())
  tipG    <- filter(y,percent>0)
  tipG_by_speed <- tipG %>% group_by(floor(speed*2)*0.5) %>% summarise(speedSum=sum(speed), count=n())
  
  for (vName in myList) {
    assign(paste0(vName,months[month]), get(vName))
  }  
  dump(paste0(myList,months[month]),file='yellow_taxi_dump.R',append=T)
  rm(x,y)

  return(myList)
}

#############################
#The following is for a fresh R session (or rm(list=lm()) to clean the current session) after storing all the parsed and filtered data on the local disc 

#############################


# Two utility functions to help us to aggregate the information from the yellow_taxi_dump.R file

Averaging.yellow.taxi<-function(df) {
  
  colKeys<-names(df)
  try(if (!('count' %in% colKeys)) stop("No count in the columns of df"))
  newCols<-sub('Sum','',colKeys)

  x<-grep(pattern='Sum$',colKeys,value=T)  # grep the column names which end by 'Sum'
  y<-as.data.frame(matrix(nrow=nrow(df),ncol=length(newCols)))
  names(y)<-newCols
  for (key in colKeys) {
    if (key %in% x) {
      headStr<-sub('Sum','',key)  #collapsing the 'Sum' into an empty string
      y[[headStr]]<-ifelse(df$count>0,df[[key]]/df$count,numeric(nrow(df)))
    }
    else {y[,key] <- df[,key]}
  }
  return(y)
}

Assign.yellow.taxi<-function(strName,months=1:12) {   # the end result is the sum of the tables from the various months
  
  monthStr <- str_pad(months,2,"left",pad='0')
  x        <- get(paste0(strName,monthStr[1]))
  colNames <- colnames(x)
  numColNames <- grep(pattern='Sum$',colNames,value=T)
  numColNames <- c(numColNames, 'count')
  keyNames    <- setdiff(colNames, numColNames)
  for (monthToken in monthStr[2:length(months)]) {
    
    y<-get(paste0(strName,monthToken))
    # Do the sums for numerical columns but taking care of the missing rows by using outer_join
    w<-full_join(x,y,keyNames)
    w[is.na(w)]<-0
    x<-w[,c(keyNames, paste0(numColNames,'.x'))]
    y<-w[,paste0(numColNames,'.y')]
    names(x) <- colNames
    names(y) <- numColNames
    x[,numColNames]<-x[,numColNames] + y    
  }    
  
  return(x)
}

