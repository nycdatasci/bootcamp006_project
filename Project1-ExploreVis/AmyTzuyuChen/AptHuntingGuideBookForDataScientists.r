##################################
##       Amy Tzu-Yu Chen        ##
##    R Visualization Project   ##
##################################

library(ggplot2)
library(dplyr)
library(gridExtra)
########### data Keyword:data scientist ##################
########### as of July 13, 2016 15:21   ##################
region<- c('New York, NY', 'Los Angeles, CA','San Jose, CA','San Francisco, CA', 'Seattle, WA',
           'Boston, MA', 'Philadelphia, PA', 'Chicago, IL', 'Dallas, TX', 'Washington, DC',
           'Houston, TX','Atlanta, GA')
Indeed<- cbind(region,c(2166,368,1354,1826,1248,1608,646,523,192,1619,189,335),rep("Indeed",12))
Linkedin<- cbind(region,c(1652,933,2216,2629,1971,1667,642,1342,659,2222,247,751),rep("LinkedIn",12))
Glassdoor<- cbind(region,c(326,100,280,361,150,159,74,126,66,245,31,91),rep("Glassdoor",12))

job<- data.frame(rbind(Indeed,Linkedin,Glassdoor))
colnames(job) <- c("Region","Jobs","sourcesite") 
job[,2]<-as.numeric(as.character(job[,2]))
temp<- group_by(job, Region) %>% summarise(total = sum(Jobs))
job <- merge(job, temp, by = 'Region') %>% arrange(sourcesite, -total)

jobplot<-ggplot(job, aes(x = reorder(Region, total),y=Jobs)) +
  geom_bar(aes(fill = sourcesite), stat='identity')+
  scale_fill_discrete(name ="Job Hunting Site")+
  ggtitle(expression(atop("Data Scientist Jobs",  
                          atop(italic("Keyword:Data Scientist, Searched on July 13, 2016"),""))))+
  ylab("Number of Jobs")+
  xlab("Metropolitan Area")+
  coord_flip()
jobplot

########### Cost of Living Indexes ##################
########### http://www.numbeo.com/cost-of-living/country_result.jsp?country=United+States ##################
cot<- cbind(region,c(100,78.62,79.51,94.82,84.34,83.72,82.39,84.99,69.26,94.02,71.41,72.90),rep("Cost of Living Index",12))
rent<- cbind(region,c(100,68.05,86.05,125.92,63.10,76.06,50.59,57.06,42.87,77.84,46.72,41.01),rep("Rent Index",12))
grocery<- cbind(region,c(100,76.29,82.00,102.72,84.72,82.15,83.46,83.93,62.63,87.10,67.86,72.24),rep("Groceries Index",12))
restaurant<-cbind(region,c(100,79.65,76.16, 91.04,77.58,85.97,77.33,84.18,70.90,90.79,77.01,68.16),rep("Restaurant Index",12))

#I decided to leave out grocery,restaurant indexes after first data exploration
costliving<- data.frame(rbind(cot,rent))
colnames(costliving) <- c("Region","Index","IndexType")
costliving[,2]<-as.numeric(as.character(costliving[,2]))
costliving<-group_by(costliving, IndexType) %>% arrange(desc(Index))

#select only top six cities based on jobs availabilities
citycot<-unique(costliving$Region)[c(1,2,3,8,6,5)]
costliving2<-filter(costliving, Region %in% citycot)

#subset by IndexType
a<-costliving2[1:6,]
b<-costliving2[7:12,]

#Create barplots for Cost of Living and Rent Index respectively
cotplot<-ggplot(a, aes(x = reorder(Region,Index),y=Index)) +
  geom_bar(aes(fill=Region),stat='identity')+
  scale_fill_brewer(palette="Dark2")+
  ylim(0,130)+
  ggtitle("Cost of Living Index")+
  ylab("Index")+
  xlab("Metropolitan Area")+
  scale_x_discrete(labels=
                     c("San Jose","Boston","Seattle","D.C.","SF","NYC"))

rentplot<-ggplot(b, aes(x = reorder(Region,Index),y=Index)) +
  geom_bar(aes(fill=Region),stat='identity')+
  scale_fill_brewer(palette="Dark2")+
  ylim(0,130)+
  ggtitle("Rent Index")+
  ylab("Index")+
  xlab("Metropolitan Area")+
  scale_x_discrete(labels=
                     c("Seattle","Boston","D.C.","San Jose","NYC","SF"))

grid.arrange(cotplot,rentplot,ncol=2,
             main="Cost of Living and Rent Index Comparison")

###########Zillow Median Rent Data##################
########### http://www.zillow.com/research/data/####
#Data Cleaning 

#MetroStudio <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPrice_Studio.csv", stringsAsFactors=FALSE)
Metro1Bedroom <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPrice_1Bedroom.csv", stringsAsFactors=FALSE)
Metro2Bedroom <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPrice_2Bedroom.csv", stringsAsFactors=FALSE)
Metro3Bedroom <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPrice_3Bedroom.csv", stringsAsFactors=FALSE)
#Metro4Bedroom <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPrice_4Bedroom.csv", stringsAsFactors=FALSE)
#Metro5Bedroom <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPrice_5BedroomOrMore.csv", stringsAsFactors=FALSE)

#MetroStudioPer <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPricePerSqft_Studio.csv", stringsAsFactors=FALSE)
Metro1BedroomPer <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPricePerSqft_1Bedroom.csv", stringsAsFactors=FALSE)
Metro2BedroomPer <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPricePerSqft_2Bedroom.csv", stringsAsFactors=FALSE)
Metro3BedroomPer <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPricePerSqft_3Bedroom.csv", stringsAsFactors=FALSE)
#Metro4BedroomPer <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPricePerSqft_4Bedroom.csv", stringsAsFactors=FALSE)
#Metro5BedroomPer <- read.csv("~/datascience/Project 1-Visuaization/Metro_MedianRentalPricePerSqft_5BedroomOrMore.csv", stringsAsFactors=FALSE)

#city contains all 12 cities and cityrent contains only cities in plot
city<-Metro1Bedroom$RegionName[c(2,3,4,5,6,7,8,10,11,12,16,34)]
cityrent<-city[c(1,7,9,10,11,12)]

#MetroStudio<-filter(MetroStudio, RegionName %in% cityrent)
Metro1Bedroom<-filter(Metro1Bedroom, RegionName %in% cityrent)
Metro2Bedroom<-filter(Metro2Bedroom, RegionName %in% cityrent)
Metro3Bedroom<-filter(Metro3Bedroom, RegionName %in% cityrent)
#Metro4Bedroom<-filter(Metro4Bedroom, RegionName %in% cityrent)
#Metro5Bedroom<-filter(Metro5Bedroom, RegionName %in% cityrent)

#MetroStudioPer<-filter(MetroStudioPer, RegionName %in% cityrent)
Metro1BedroomPer<-filter(Metro1BedroomPer, RegionName %in% cityrent)
Metro2BedroomPer<-filter(Metro2BedroomPer, RegionName %in% cityrent)
Metro3BedroomPer<-filter(Metro3BedroomPer, RegionName %in% cityrent)
#Metro4BedroomPer<-filter(Metro4BedroomPer, RegionName %in% cityrent)
#Metro5BedroomPer<-filter(Metro5BedroomPer, RegionName %in% cityrent)

#drop col 2010.01 in 2 and 3 bdroom
Metro2Bedroom<-Metro2Bedroom[,-3]
Metro3Bedroom<-Metro3Bedroom[,-3]
Metro2BedroomPer<-Metro2BedroomPer[,-3]
Metro3BedroomPer<-Metro3BedroomPer[,-3]

cityrent<-Metro1Bedroom$RegionName
time<-colnames(Metro1Bedroom)[-c(1,2)]
time<-substring(time,2,nchar(time))
year<-substring(time,1,4)
month<-substring(time,6,7)

allRoom<- data.frame(Region= rep(rep(cityrent,each=length(time)),3),
                     Time= rep(rep(time,6),3),
                     Year= rep(rep(year,6),3),
                     Month= rep(rep(month,6),3),
                     Price=numeric(1368),
                     RoomType=c(rep('1Bedroom',6*length(time)),
                                rep('2Bedroom',6*length(time)),
                                rep('3Bedroom',6*length(time))))
allRoom$Price[1:(6*length(time))]<-as.numeric(c(Metro1Bedroom[1,3:(length(time)+2)],
                                     Metro1Bedroom[2,3:(length(time)+2)],
                                     Metro1Bedroom[3,3:(length(time)+2)],
                                     Metro1Bedroom[4,3:(length(time)+2)],
                                     Metro1Bedroom[5,3:(length(time)+2)],
                                     Metro1Bedroom[6,3:(length(time)+2)]))
allRoom$Price[(6*length(time))+1:(12*length(time))]<-as.numeric(c(Metro2Bedroom[1,3:(length(time)+2)],
                                                       Metro2Bedroom[2,3:(length(time)+2)],
                                                       Metro2Bedroom[3,3:(length(time)+2)],
                                                       Metro2Bedroom[4,3:(length(time)+2)],
                                                       Metro2Bedroom[5,3:(length(time)+2)],
                                                       Metro2Bedroom[6,3:(length(time)+2)]))
allRoom$Price[((12*length(time))+1):(18*length(time))]<-as.numeric(c(Metro3Bedroom[1,3:(length(time)+2)],
                                                          Metro3Bedroom[2,3:(length(time)+2)],
                                                          Metro3Bedroom[3,3:(length(time)+2)],
                                                          Metro3Bedroom[4,3:(length(time)+2)],
                                                          Metro3Bedroom[5,3:(length(time)+2)],
                                                          Metro3Bedroom[6,3:(length(month)+2)]))

perRoom<- data.frame(Region= rep(rep(cityrent,each=length(time)),3),
                     Time= rep(rep(time,6),3),
                     Year= rep(rep(year,6),3),
                     Month= rep(rep(month,6),3),
                     Price=numeric(1368),
                     RoomType=c(rep('1Bedroom',6*length(time)),
                                rep('2Bedroom',6*length(time)),
                                rep('3Bedroom',6*length(time))))
perRoom$Price[1:(6*length(time))]<-as.numeric(c(Metro1BedroomPer[1,3:(length(time)+2)],
                                      Metro1BedroomPer[2,3:(length(time)+2)],
                                      Metro1BedroomPer[3,3:(length(time)+2)],
                                      Metro1BedroomPer[4,3:(length(time)+2)],
                                      Metro1BedroomPer[5,3:(length(time)+2)],
                                      Metro1BedroomPer[6,3:(length(time)+2)]))
perRoom$Price[(6*length(time))+1:(12*length(time))]<-as.numeric(c(Metro2BedroomPer[1,3:(length(time)+2)],
                                                         Metro2BedroomPer[2,3:(length(time)+2)],
                                                         Metro2BedroomPer[3,3:(length(time)+2)],
                                                         Metro2BedroomPer[4,3:(length(time)+2)],
                                                         Metro2BedroomPer[5,3:(length(time)+2)],
                                                         Metro2BedroomPer[6,3:(length(time)+2)]))
perRoom$Price[((12*length(time))+1):(18*length(time))]<-as.numeric(c(Metro3BedroomPer[1,3:(length(time)+2)],
                                                            Metro3BedroomPer[2,3:(length(time)+2)],
                                                            Metro3BedroomPer[3,3:(length(time)+2)],
                                                            Metro3BedroomPer[4,3:(length(time)+2)],
                                                            Metro3BedroomPer[5,3:(length(time)+2)],
                                                            Metro3BedroomPer[6,3:(length(time)+2)]))

#plots of total and unit price trend from 2015/01 to 2016/05
#Cities: San Jose, CA, New York, NY, Seattle, WA, San Francisco, CA
allRoom2<-filter(allRoom, Year %in% c(2015,2016),
                 Region %in% c("San Jose, CA","New York, NY","Seattle, WA","San Francisco, CA"))
perRoom2<-filter(perRoom, Year %in% c(2015,2016),
                 Region %in% c("San Jose, CA","New York, NY","Seattle, WA","San Francisco, CA"))

allplot<-ggplot(allRoom2, aes(x = Time,y= Price, group=Region))+
  geom_line(aes(colour=Region)) +ylim(1200,4000)+facet_wrap(~RoomType)+
  scale_fill_brewer(palette="Dark2")+
  ggtitle("Median Rental Price by Room Type")+
  scale_x_discrete(labels=c("15/1","2","3","4","5","6","7","8","9","10","11","12",
                            "16/1","2","3","4","5"))
allplot

perplot<-ggplot(perRoom2, aes(x = Time, y= Price, group=Region))+
  geom_line(aes(colour=Region)) +facet_wrap(~RoomType)+
  scale_fill_brewer(palette="Dark2")+
  ggtitle("Median Rental Price per Sqrt by Room Type")+
  scale_x_discrete(labels=c("15/1","2","3","4","5","6","7","8","9","10","11","12",
                            "16/1","2","3","4","5"))
perplot

##month peak graphs
#Cities: New York, DC, Boston, San Francisco, Seattle, San Jose   
maxmonth<-allRoom %>% 
  group_by(Region,RoomType,Year) %>% 
  filter(Price==max(Price))

minmonth<-allRoom %>% 
  group_by(Region,RoomType,Year) %>% 
  filter(Price==min(Price))

maxplot<-ggplot(maxmonth, aes(x = Month)) +
  geom_bar(aes(fill = RoomType), stat='bin')+
  scale_fill_discrete(name ="Room Type")+
  ggtitle("Maximum Rent Occurrence")+
  ylab("Maximum Rent Occurence Count")+
  scale_fill_brewer(palette="Paired")+
  xlab("Month")+
  ylim(0,60)

minplot<-ggplot(minmonth, aes(x = Month)) +
  geom_bar(aes(fill = RoomType), stat='bin')+
  scale_fill_discrete(name ="Room Type")+
  ggtitle("Minimum Rent Occurrence")+
  ylab("Minimum Rent Occurence Count")+
  scale_fill_brewer(palette="Paired")+
  xlab("Month")+
  ylim(0,60)

grid.arrange(maxplot,minplot,ncol=2,
             main="In What Months did Max and Min Rent Occur?")
