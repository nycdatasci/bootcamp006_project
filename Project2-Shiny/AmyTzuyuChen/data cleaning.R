##################################
##       Amy Tzu-Yu Chen        ##
##       R Shiny Project        ##
##################################
library(ggmap)
library(dplyr)

##################use address to get long/lat info################
kinder <- read.csv(url("https://github.com/amy17519/NYCPre-KGuide/blob/master/Universal_Pre-K__UPK__School_Locations.csv"), 
                   stringsAsFactors=FALSE)

######paste zipcodes to all addresses to increase accuracy of locating
kinder$address<- paste(kinder$address,kinder$zip)
geo<-geocode(kinder$address)
geo<-geo[,c(2,1)]   #change col order of long and lat to match convention 

#Find abnormal locations using longitude/latitude boundaries of NYC
#Northernmost pt:40.916541, -73.906637
#Southernmost pt:40.525095, -74.246319
#Easternmost pt:40.722813, -73.688498
#Westernmost pt:40.510940, -74.253454
#####there are 18 suspicious locations that might not be in NYC
index<-which(geo$lat>=40.916541 | geo$lat<= 40.525095 |
             geo$lon>=-73.688498 | geo$lon<= -74.253454)

#change locations of those outside of boundaries
geo[index[1],]<-c(40.669229,-73.948583)
geo[index[2],]<-c(40.636565, -74.009728) #full address is 1313 Union St 11225
geo[index[3],]<-c(40.693324, -73.912429)
geo[index[4],]<-c(40.748108, -73.906311)
geo[index[5],]<-c(40.758443, -73.817811)
geo[index[6],]<-c(40.701409, -73.789412)
geo[index[7],]<-c(40.673309, -73.763930)  #address is also 137-37 Farmers Blvd 11434
geo[index[8],]<-c(40.750195, -73.803728)
geo[index[9],]<-c(40.780474, -73.844827)  #address is also 123-07 22 Ave 11356
geo[index[10],]<-c(40.666244, -73.768001)
geo[index[11],]<-c(40.763887, -73.922587)
geo[index[12],]<-c(40.762018, -73.827467) #full address is 3722 Union St 11354
geo[index[13],]<-c(40.613016, -74.075271) #full address is 58 Smith St 10307
geo[index[14],]<-c(40.521021, -74.211909)
geo[index[15],]<-c(40.510519, -74.230451)
geo[index[16],]<-c(40.613424, -74.075527) #full address is 23 Smith St 10307
geo[index[17],]<-c(40.509204, -74.246334)
geo[index[18],]<-c(40.512266, -74.222494)

#some missing rows due to geocode's daily limit on API requests
geo[677,]<-c(40.718359, -74.010878)
geo[678,]<-c(40.860254, -73.927580)
geo[679,]<-c(40.826704, -73.947511)
geo[680,]<-c(40.810734, -73.948852)
geo[681,]<-c(40.797698, -73.936193)
geo[682,]<-c(40.770382, -73.950719)
geo[683,]<-c(40.795132, -73.968481)
geo[684,]<-c(40.802832, -73.965939)
geo[685,]<-c(40.793974, -73.948572)
geo[686,]<-c(40.792523, -73.951364)
geo[722,]<-c(40.794031, -73.948529)
geo[725,]<-c(40.792474, -73.951492)
geo[797,]<-c(40.860295, -73.927591)

#some places in Statan Island were misplaced
geo[1846,]<-c(40.811843, -73.905739)
geo[1452,]<-c(40.512355, -74.222430)
geo[1366,]<-c(40.521021, -74.211888)
geo[1368,]<-c(40.510478, -74.230440)
geo[1451,]<-c(40.509220, -74.246388)


kinder<-cbind(kinder,geo)       #add location info to pre-k dataset
write.csv(geo,file='geo.csv')   #back up location info


############change values of dataset to self-explanatory version###########
kinder$PreK_Type[kinder$PreK_Type =='CHARTER']<-'Charter'

kinder$Borough[kinder$Borough =='M']<-'Manhattan'
kinder$Borough[kinder$Borough =='K']<-'Brooklyn'
kinder$Borough[kinder$Borough =='Q']<-'Queens'
kinder$Borough[kinder$Borough =='X']<-'Bronx'
kinder$Borough[kinder$Borough =='R']<-'Staten Island'

kinder$Length[kinder$Day_Length ==1]<-'Full Day'
kinder$Length[kinder$Day_Length ==2]<-'Both Full and Half Day'
kinder$Length[kinder$Day_Length ==3]<-'5-Hour'
kinder$Length[kinder$Day_Length ==4]<-'Both Full and 5-Hour'
kinder$Length[kinder$Day_Length ==5]<-'Both Half Day and 5-Hour'
kinder$Length[kinder$Day_Length ==6]<-'Full Day, Half Day and 5-Hour'

kinder$Meal_Plan[kinder$MEALS ==1]<-'Breakfast'
kinder$Meal_Plan[kinder$MEALS ==2]<-'Breakfast/Lunch'
kinder$Meal_Plan[kinder$MEALS ==3]<-'Breakfast/Lunch/Snacks'
kinder$Meal_Plan[kinder$MEALS ==4]<-'Breakfast/Snacks'
kinder$Meal_Plan[kinder$MEALS ==5]<-'Contact agency'
kinder$Meal_Plan[kinder$MEALS ==6]<-'Lunch'
kinder$Meal_Plan[kinder$MEALS ==7]<-'Lunch/Snacks'
kinder$Meal_Plan[kinder$MEALS ==8]<-'Snacks'

kinder$Playspace[kinder$INDOOR_OUTDOOR ==1]<-'Indoor'
kinder$Playspace[kinder$INDOOR_OUTDOOR ==2]<-'Indoor/Outdoor (offsite)'
kinder$Playspace[kinder$INDOOR_OUTDOOR ==3]<-'Indoor/Outdoor (onsite) '
kinder$Playspace[kinder$INDOOR_OUTDOOR ==4]<-'Indoor/Outdoor (onsite) and Outdoor (offsite)'
kinder$Playspace[kinder$INDOOR_OUTDOOR ==5]<-'None'
kinder$Playspace[kinder$INDOOR_OUTDOOR ==6]<-'Outdoor (offsite)'
kinder$Playspace[kinder$INDOOR_OUTDOOR ==7]<-'Outdoor (onsite)'
kinder$Playspace[kinder$INDOOR_OUTDOOR ==8]<-'Outdoor (onsite/offsite)'
kinder$Playspace[kinder$INDOOR_OUTDOOR ==9]<-'Contact agency'

kinder$Extended_Day_Care[kinder$EXTENDED_DAY ==1]<-'Offered'
kinder$Extended_Day_Care[kinder$EXTENDED_DAY ==2]<-'Not offered'
kinder$Extended_Day_Care[kinder$EXTENDED_DAY ==3]<-'Contact agency'

special_req<-levels(as.factor(kinder$NOTE))
kinder$Priority[kinder$NOTE ==special_req[1]]<-'None'
kinder$Priority[kinder$NOTE ==special_req[2]]<-'Coast Guard families'
kinder$Priority[kinder$NOTE ==special_req[3]]<-'Independent admissions. Contact school for details'
kinder$Priority[kinder$NOTE ==special_req[4]]<-'District 5 and 6'
kinder$Priority[kinder$NOTE ==special_req[5]]<-'Zones 08X014, 08X071, or 08X072'
kinder$Priority[kinder$NOTE ==special_req[6]]<-'10% seats for students impacted by incarceration; 60% seats for Free and Reduced Lunch students'
kinder$Priority[kinder$NOTE ==special_req[7]]<-'20% seats for English learners and students in child welfare system. Contact school for details'
kinder$Priority[kinder$NOTE ==special_req[8]]<-'45% seats for English learners and students in child welfare system. Contact school for details'
kinder$Priority[kinder$NOTE ==special_req[9]]<-'Two-thirds seats for District 15, One-third seats for District 13 (35% for English learners and students in child welfare system)'
kinder$Priority[kinder$NOTE ==special_req[10]]<-'Income and other eligibility requirements apply'
kinder$Priority[kinder$NOTE ==special_req[11]]<-'Free and Reduced Lunch students after siblings'

kinder_view<-kinder[,c(2,3,4,6,7,13,14,19,20,8,10,21,22,23,24,25)]  #save only variables needed for Shiny
write.csv(kinder_view,file="kid.csv")   #save the cleaned dataset as a csv file

################ dataset for GoogleVis charts ############################
boroughkid<-group_by(kinder_view,Borough)  %>% 
  summarise(totalseats=sum(Seats))

#source: http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=CF
#2014 est and 2010 census of children who are 0-5 years old in each borough
boroughkid$est2014<-c(106806,189435,81666,140447,27938)/5
boroughkid$pop2010<-c(103144,177198,76579,132464,28339)/5
boroughkid[6,]<-c('All Boroughs',sum(boroughkid$totalseats),sum(boroughkid$est2014),sum(boroughkid$pop2010))

write.csv(boroughkid,file="boroughkid.csv") 