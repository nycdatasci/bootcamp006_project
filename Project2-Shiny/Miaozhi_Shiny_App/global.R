setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Shiny/Realty_price/Shiny_App')
library('ggmap')
library('mapproj')
library('leaflet')
library('shiny')
library(dplyr)
library(dygraphs)
library(xts)
source('Shiny.R')

#load data
lo = read.csv('./data/lo.csv')
long_lat = read.csv('./data/long_lat.csv')
long_lat2 = read.csv('./data/long_lat2.csv')
long_lat3 = read.csv('./data/long_lat3.csv')
long_lat4 = read.csv('./data/long_lat4.csv')
geoloc = rbind(lo,long_lat,long_lat2,long_lat3,long_lat4)
write.csv(geoloc,'geoloc.csv')
#geoloc = read.csv('geoloc.csv')
#create clean data set
#geoloc = append(long_lat,long_lat2)
#df1 = read.csv('lon1.csv')
x=c()
for (i in 1:4209){
  if((geoloc$lon[i]<(-74.354095))|(geoloc$lon[i]>(-73.390045))|(geoloc$lat[i]<40.488150 )|(geoloc$lat[i]>41.022844)){
    x=rbind(x,i)
  }
}
newdata = data.frame(mydata,cost,cost.FT2,lon = geoloc$lon,lat = geoloc$lat)
geoloc=geoloc[-x,]
finaldata=newdata[-x,]
bed=levels(finaldata$BEDS)
bath=levels(finaldata$BATHS)
reg = levels(finaldata$NEIGHBORHOOD)

filter(finaldata, finaldata$NEIGHBORHOOD=='Bath Beach')

boroughs = data.frame(NEIGHBORHOOD = reg, 
                      area = c('','Brooklyn','Financial District','Brooklyn','Brooklyn',
                               'Midtown','Brooklyn','Brooklyn','Brooklyn','Brooklyn',
                               'Brooklyn','Upper West Side','Brooklyn','Brooklyn','Brooklyn',
                               'Upper East Side','Brooklyn','Upper West Side','Downtown','Brooklyn',
                               'Brooklyn','Brooklyn','Brooklyn','Brooklyn','Brooklyn',
                               'Upper Manhattan','Brooklyn','Downtown','Financial District','Brooklyn',
                               'Downtown','Brooklyn','Brooklyn','Brooklyn','Downtown',
                               'Brooklyn','Brooklyn','Downtown','Downtown','Upper Manhattan',
                               'Upper Manhattan','Brooklyn','Upper Manhattan','Brooklyn','Upper West Side',
                               'Upper West Side','Downtown','Brooklyn','Midtown','Midtown',
                               'Brooklyn','Upper West Side','Midtown','Brooklyn','Downtown',
                               'Downtown','Brooklyn','Upper East Side','Brooklyn','Brooklyn',
                               'Brooklyn','Upper West Side','Upper East Side','Brooklyn','Downtown',
                               'Brooklyn','Brooklyn','Downtown','Midtown','Upper Manhattan',
                               'Downtown','Brooklyn','Brooklyn','Upper East Side'))
final = inner_join(finaldata,boroughs)
final$NEIGHBORHOOD = as.character(final$NEIGHBORHOOD)
bor = levels(final$area)
name = data.frame(final$ADDRESS,final$lon,final$lat)
final$SALE.DATE = as.Date(final$SALE.DATE, '%d-%b-%y')
xt = xts(x=final$cost.FT2, order.by = final$SALE.DATE)

# print(head(subset))
group = group_by(final,area,SALE.DATE) %>% summarise(avg = mean(cost.FT2,na.rm=T),count=n())
Brooklyn=xts(x=group$avg[group$area=="Brooklyn"],order.by = group$SALE.DATE[group$area=="Brooklyn"])
# dygraph(xt1)
Midtown=xts(x=group$avg[group$area=="Midtown"],order.by = group$SALE.DATE[group$area=="Midtown"])
Downtown=xts(x=group$avg[group$area=="Downtown"],order.by = group$SALE.DATE[group$area=="Downtown"])
Financial_District=xts(x=group$avg[group$area=="Financial District"],order.by = group$SALE.DATE[group$area=="Financial District"])
Upper_East_Side=xts(x=group$avg[group$area=="Upper East Side"],order.by = group$SALE.DATE[group$area=="Upper East Side"])
Upper_Manhattan=xts(x=group$avg[group$area=="Upper Manhattan"],order.by = group$SALE.DATE[group$area=="Upper Manhattan"])
Upper_West_Side=xts(x=group$avg[group$area=="Upper West Side"],order.by = group$SALE.DATE[group$area=="Upper West Side"])

all = cbind(Midtown,Brooklyn,Downtown,Upper_West_Side,Upper_Manhattan,Upper_East_Side,Financial_District)
all
