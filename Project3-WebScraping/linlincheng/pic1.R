

install.packages('tm')
isntall.packages('wordcloud')

library('tm')
library('wordcloud')
library('SnowballC')
library('dplyr')
library('lubridate')
library('ggmap')
library(maptools)
library(maps)
library('ggplot2')


#############
#import dataset
event_list <- read.csv("~/event_list.csv")
#############
############
#plot1. locations on the map:
#find a map to locate the auction places
visited <- c("Amsterdam", "Dubai", "Geneva", "Berlin", "Glasgow",
              "Hong Kong", "London","Los Angeles", "Madrid", 
             "Mallorca", "Melbourne", "Milan", "Monaco", "Mumbai", 
             "Paris", "Rome", "Shanghai", "Singapore", "Sydney", 
             "Taipei", "Tel Aviv", "Zurich", "New York")

ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="purple", alpha =0.5, size=3)+xlab('')+ylab('')+
    ggtitle("Christie's Auction Houses by Location")
mp




#
class(event_list$total_sales) #found out to be factors
#converting total_sales to numbers
event_list$total_sales <- as.numeric(gsub(",","", event_list$total_sales))

#converting currencies to US Dollars:
levels(event_list$location) #print levels of the locations
loc <- event_list$location
sales_tmp <- event_list$total_sales
yr<-event_list$year

#multiply by exchange rate on Google, as of Aug 16, 2016
sales_tmp<-ifelse(loc == 'Amsterdam'|loc == 'Germany'|loc =='Madrid'|loc == 'Mallorca'| loc == 'Milan'| loc== 'Monaco'|
               loc == 'Paris'| loc == 'Rome', round(sales_tmp* 1.12),sales_tmp)
sales_tmp<-ifelse(loc == 'Dubai', round(sales_tmp* 0.27), sales_tmp)
sales_tmp<-ifelse(loc == 'Geneva' | loc == 'Zurich', round(sales_tmp* 1.03), sales_tmp)
sales_tmp<-ifelse(loc == 'Hong Kong', round(sales_tmp* 0.13), sales_tmp)
sales_tmp<-ifelse(loc == 'Glasgow' |loc == 'London, King Street'| loc == 'London, South Kensington'|
         loc == 'Spink London', round(sales_tmp* 0.13), sales_tmp)
sales_tmp<-ifelse(loc == 'Melbourne' | loc == 'Spink Australia' |loc == 'Sydney', round(sales_tmp* 0.77), sales_tmp)
sales_tmp<-ifelse(loc == 'Mumbai', round(sales_tmp* 0.015), sales_tmp)
sales_tmp<-ifelse(loc == 'Singapore' | loc =='Spink Singapore', round(sales_tmp* 0.74), sales_tmp)
sales_tmp<-ifelse(loc == 'Shanghai', round(sales_tmp* 0.15), sales_tmp)
sales_tmp<-ifelse(loc == 'Taipei', round(sales_tmp* 0.032), sales_tmp)
sales_tmp<-ifelse(loc == 'Tel Aviv', round(sales_tmp* 0.26), sales_tmp)

#pre euro conversion
#if time [01/1998-04/2012]
#       Germany: sales_tmp/1.95583
#       Amsterdam: /2.20371
#       Madrid|Mallorca: /166.386
#       Paris|Monaco:/6.55957
#       Rome|Milan: /1,936.27	

##       
event_list$time2 <- as.Date(paste(event_list$year, event_list$month, event_list$date, sep="-"))
#order by time 
#event_list2<-event_list[order(as.Date(event_list$time2, format="%d-%m-%Y")),]   

#date1 <- as.Date("1998-01-01")
#date2 <- as.Date("2002-05-01")
#int <- new_interval(date1, date2)
#event_list2[event_list2$time2 %within% int,]
      
event_list2$time2<'2002-05-01'
ymd<-event_list2$time2

sales_tmp<-ifelse(loc == 'Amsterdam' & event_list2$time2<'2002-05-01',round(sales_tmp/2.20371),sales_tmp )
sales_tmp<-ifelse(loc == 'Madrid' & event_list2$time2<'2002-05-01',round(sales_tmp/166.386),sales_tmp )
sales_tmp<-ifelse(loc == 'Mallorca' & event_list2$time2<'2002-05-01',round(sales_tmp/166.386),sales_tmp )
sales_tmp<-ifelse(loc == 'Germany' & event_list2$time2<'2002-05-01',round(sales_tmp/1.95583),sales_tmp )
sales_tmp<-ifelse(loc == 'Milan' & event_list2$time2<'2002-05-01',round(sales_tmp/1936.27),sales_tmp )
sales_tmp<-ifelse(loc == 'Rome' & event_list2$time2<'2002-05-01',round(sales_tmp/1936.27),sales_tmp )
sales_tmp<-ifelse(loc == 'Paris' & event_list2$time2<'2002-05-01',round(sales_tmp/6.55957),sales_tmp )
sales_tmp<-ifelse(loc == 'Monaco' & event_list2$time2<'2002-05-01',round(sales_tmp/6.55957),sales_tmp )




#replacing converted sales into the original dataframe
event_list$tota_sales <- sales_tmp
event_list <- cbind(event_list, sales_tmp)

#find the maximum sales event by year:
max_sales <- event_list %>% group_by(year) %>% summarise(Value = max(sales_tmp))
max_sales_by_year = event_list[which(event_list$tota_sales %in% max_sales$Value),]
#order by year
arrange(max_sales_by_year, desc(year))
#order by total_sales
arrange(max_sales_by_year, desc(tota_sales))

#joining year, month and date strings for a time stamp variable
event_list$month<-match(event_list$month,month.abb)
event_list$time <- paste(event_list$date, event_list$month, event_list$year, sep="/")
#order by time 
event_list[order(as.Date(event_list$time, format="%d-%m-%Y")),]

#create time series plot for overall number of events for later use:
total_year <- event_list %>% group_by(year) %>%summarise(n = n())

#create series of events by places for later:
total_year_eve <- event_list %>% group_by(location) %>%summarise(n = n())

#Plot6
#for specific places
total_location<-group_by(event_list, location, year)%>%summarise(n = n())
   ##scatter plot with jitter? color by location
total_location[1:3]
ggplot(total_location,aes(x=year,y=n,colour=location,group=location)) + geom_line()+
       xlab('')+ylab('')+ggtitle('Number of Events by Houses Over Time')

#Plot5.
#find out overall total_sales distribution: time series
total_sales_dis <- event_list %>% group_by(year) %>%summarise(global_sales = sum(sales_tmp))
ggplot(total_sales_dis, aes(year, global_sales)) + geom_line() +
  xlab("") + ylab("")+ggtitle('Global Sales Over Time')#driven by auctions in Rome




#Preparing dataframe for Plot2-4
#find out the sales distribution for each place
total_location_dis <- event_list %>% group_by(location) %>%summarise(global_sales = sum(sales_tmp))
df_year_loc <-cbind(total_location_dis, num_events=total_year_eve$n)
avg<-df_year_loc$global_sales/df_year_loc$num_events
df_year_loc2<-cbind(df_year_loc, avg_sales=avg) 


#Plot2
#sort by average sales for each location
df1<-arrange(df_year_loc2, desc(avg_sales))[1:5,] #marlloca #1
#rearranging the bars
df1_<-factor(df1$location, levels = c("Rome", "Monaco", "Geneva", "Hong Kong", "New York, Rockefeller Plaza"))
ggplot(data=df1, aes(x=df1_, y=avg_sales, fill=avg_sales)) +
  geom_bar(stat="identity")+xlab('')+ylab('')+ggtitle('Top 5 Average Sales by Location')


#Plot3
#sort by num_events:
df2<-arrange(df_year_loc2, desc(num_events))[1:5,] #London, South Kensington #1
df2_<-factor(df2$location, levels = c("London, South Kensington", "London, King Street", "New York, Rockefeller Plaza", "Amsterdam", "Paris"))
ggplot(data=df2, aes(x=df2_, y=num_events, fill=num_events)) +
  geom_bar(stat="identity")+xlab('')+ylab('')+ggtitle('Top 5 Busiest Houses by Location')

#Plot4
#sort by global_sales
df3<-arrange(df_year_loc2, desc(global_sales))[1:5,] #Rome #1
df3_<-factor(df3$location, levels = c("Rome", "New York, Rockefeller Plaza", "Hong Kong", "Geneva", "Paris"))

ggplot(data=df3, aes(x=df3_, y=global_sales), fill=global_sales)+
       geom_bar(stat="identity")+xlab('')+ylab('')+ggtitle('Top 5 Revenue Generating Houses')



#find out price distribution of the items
#find out the key words of auctioned items 

