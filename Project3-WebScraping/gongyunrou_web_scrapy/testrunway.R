setwd("~/Desktop/gongyunrou_web_scrapy")
library(rmongodb)
library(plyr)
library(dplyr)
library(googleVis)
library(ggplot2)
# connect to mongodb
mongo <- mongo.create()
mongo.is.connected(mongo)
mongo.get.databases(mongo)
# connect to database testrunway
mongo.get.database.collections(mongo, db = "testrunway")
# size of collection of dresses
mongo.count(mongo, "testrunway.dresses") # 2702
# fineOne Item
tmp.one <-  mongo.find.one(mongo, "testrunway.dresses")
# tmp.one <-  mongo.bson.to.list(tmp.one)
# tmp.one

# find all items 
find_all <-  mongo.find.all(mongo, ns = "testrunway.dresses")
find_all.df <- as.data.frame(Reduce(rbind,t(find_all)))

# export the data and transfer the data type
item=as.character(unlist(find_all.df$name))
brand=as.factor(unlist(find_all.df$designer))
reviews=as.integer(unlist(find_all.df$numOfReviews))
avgRating=as.numeric(unlist(find_all.df$avgRating))
fit_large=as.integer(unlist(find_all.df$fit_large))
true_to_size=as.integer(unlist(find_all.df$true_to_size))
fit_small=as.integer(unlist(find_all.df$fit_small))
img_url= as.character(unlist(find_all.df$img))
retailPrice=as.numeric(unlist(find_all.df$retailPrice))
rentalPrice_4=as.numeric(unlist(find_all.df$rentalPrice_4))
rentalPrice_8=as.numeric(unlist(find_all.df$rentalPrice_8))

# create dataframe for analysis
rwdata <- cbind.data.frame(item,brand,reviews,avgRating,
                         fit_large,true_to_size,fit_small,img_url,
                         retailPrice,rentalPrice_4,rentalPrice_8)                               




# ratio of rental4/retail and rental8/retail, rental_4 vs rental 8 
rwdata$ratio_4 <- round((rwdata$rentalPrice_4/rwdata$retailPrice)*100,2)
rwdata$ratio_8 <- round((rwdata$rentalPrice_8/rwdata$retailPrice)*100,2)
rwdata$increase_rate <- (rwdata$rentalPrice_8-rwdata$rentalPrice_4)/rwdata$rentalPrice_4
rwdata <- rwdata[-14]
#impute the missing data by the rate of increasing of 4 days rental price to 8 days rental price
rwdata[is.na(rwdata$rentalPrice_8),"rentalPrice_8"]  <- 
  rwdata[is.na(rwdata$rentalPrice_8),"rentalPrice_4"]*(1+0.6)
rwdata[rwdata$rentalPrice_4==rwdata$rentalPrice_8,"rentalPrice_8"] <- 
  rwdata[rwdata$rentalPrice_4==rwdata$rentalPrice_8,"rentalPrice_4"]*(1+0.6)

# write.csv(rwdata,file="runwaydata.csv",row.names = F)
# sub categorise 
rwdata$item <- as.character(rwdata$item)
gownSubset <- rwdata[grep("Gown",rwdata$item ), ]
maxiSubset <- rwdata[grep("Maxi",rwdata$item ), ]
sheathSubset <- rwdata[grep("Sheath",rwdata$item ),]
shiftSubset <- rwdata[grep("Shift",rwdata$item ),]
shirtSubset <- rwdata[grep("Shirt",rwdata$item ),]
frockSubset <- rwdata[grep("Frock",rwdata$item ),]
wrapSubset <- rwdata[grep("Wrap Dress",rwdata$item ),]
slipSubset <- rwdata[grep("Slip Dress",rwdata$item ),]


rwdata$type <- ifelse(rwdata$item %in% gownSubset$item, "gown", 
               ifelse(rwdata$item %in% maxiSubset$item, "maxidress",
                      ifelse(rwdata$item %in% sheathSubset$item,"sheath",
                             ifelse(rwdata$item %in% shiftSubset$item,"shift",
                                    ifelse(rwdata$item %in% shirtSubset$item,"shirtdress",
                                           ifelse(rwdata$item %in% frockSubset$item,"frock",
                                                  ifelse(rwdata$item %in% wrapSubset$item,"wrapdress",
                                                         ifelse(rwdata$item %in% slipSubset$item,"slipdress","regulardress"))))))))


rwdata$type <- as.factor(rwdata$type)

rwdata <- rwdata[-13]
rwdata <- rwdata[-12]
rwdata <- rwdata[-12]
rwdata <- rwdata%>%mutate(discount=((retailPrice-rentalPrice_4)/retailPrice)*100)

#write.csv(rwdata,file="data.csv",row.names = F)
rwdata <- read.csv("data.csv")

# linear relationship
plot(x=rwdata$rentalPrice_4,y=rwdata$rentalPrice_8)

# EDA for categorical variable 

# brand 
brand_freq <- count(rwdata, vars=brand,sort = T)
Pie <- gvisPieChart(brand_freq)
plot(Pie)

Pie2 <- gvisPieChart(brand_freq, options=list(
  slices="{25: {offset: 0.2}, 0: {offset: 0.3}}",
  title='Brand Percentage in Pie Chart',
  legend="{ position:'left'}",
  width=1000, height=600,
  pieSliceText='label',
  pieHole=0.5))
plot(Pie2)

# type 

type_share <- count(rwdata, vars=type,sort = T)
Pie.type <- gvisPieChart(type_share, options=list(
  slices="{6: {offset: 0.2}, 1: {offset: 0.3}}",
  title='Dress Type Share in Pie Chart',
  legend="{ position:'left'}",
  width=800, height=600,
  pieSliceText='label',
  pieHole=0.5))
plot(Pie.type)

# bar chart
rwdata$item <- as.character(rwdata$item)

 
# g.bar <- gvisColumnChart(itembar,xvar ="item",yvar = "reviews",
#                          options=list(colors="#99ccff",
#                                       width="1000px", height="500px",
#                                       vAxes="[{title:'Volume of Flights'}]",
#                                       hAxes="[{title:'Month'}]",
#                                       title="Top 20 Popular Dresses ",
#                                       titleTextStyle="{fontSize:18}",
#                                       legend="{position:'Top'}"))
# plot(g.bar)



##
pairs(mydata[-1])

# 

plot(density(rwdata$discount), col=c("purple"),
     main = "Overall Distribution of Heart Rate")


#

#  data by selected releverant variable to predict rental price 
mydata2 <- rwdata%>%select(item,brand,type,rentalPrice_4,retailPrice)

#
s <- gvisScatterChart(rwdata[,c(10,11)],
                      options=list(colors="hotpink",
                                   width="1000px", height="500px",
                                   vAxes="[{title:'rental Price of 8 days'}]",
                                   hAxes="[{title:'rental Price of 4 days'}]",
                                   title="ScatterPlot rentalPrice_8 Vs rentalPrice_4",
                                   titleTextStyle="{fontSize:18}",
                                   legend="{position:'Top'}"))

plot(s)

s2 <- gvisScatterChart(rwdata[,c(9,10)],
                      options=list(colors="pink",
                                   width="1000px", height="500px",
                                   vAxes="[{title:'rental Price of 4 days'}]",
                                   hAxes="[{title:'retail price of dress'}]",
                                   title="ScatterPlot rentalPrice_4 Vs retailPrice",
                                   titleTextStyle="{fontSize:18}",
                                   legend="{position:'Top'}"))


plot(s2)

## reviews VS discount 
s3 <- gvisScatterChart(rwdata[,c(3,13)],
                       options=list(colors="orange",
                                    width="1000px", height="500px",
                                    vAxes="[{title:'Discount(%)'}]",
                                    hAxes="[{title:'number of reviews'}]",
                                    title="ScatterPlot Discount Vs number of reviews",
                                    titleTextStyle="{fontSize:18}",
                                    legend="{position:'Top'}"))

plot(s3)


