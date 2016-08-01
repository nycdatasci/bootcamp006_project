#data cleaning 

GHGdata <- read.csv("./GHG.csv")
#GHGdata$Country[GHGdata$Country=='United States']<-'US'
#GHGdata$Country[GHGdata$Country=='United Kingdom']<-'UK'
#a<-ghg %>% mutate(.,Country = ifelse(Country=='United States', 'USA', paste(Country)))
colnames(GHGdata)[5]<-'Year'
saveRDS(GHGdata, file='./GHG.RDS')

#select only the columns for Variables, Country, Year, and Value
ghg<-readRDS(GHGdata,file='./GHG.RDS' )
ghg1<-ghg[,c(1,2,5,9)]
#reshaping data to be combined with economic data soon
#data_wide <- dcast(ghg, Country + Year ~ Variable, value.var="Value")
w <- reshape(ghg1, 
             timevar = "Variable",
             idvar = c("Country", "Year"),
             direction = "wide")

#http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_complete.txt
avg_tmp <- read.table("~/Desktop/shiny_app/data/avg_tmp.txt", quote="\"")[,c(1,3)]
colnames(avg_tmp)[1:3]<-c('Year', 'avg')
avg<-avg_tmp[,c(1,3)]
colnames(avg)<-c('')

#avg_tmp1<- read.table("~/Desktop/shiny_app/data/high_tmp.txt", quote="\"")[,1:3]
#colnames(avg_tmp1)[1:3]<-c('Year', 'Month', 'high')
#avg_tmp2 <- read.table("~/Desktop/shiny_app/data/low_tmp.txt", quote="\"")[,1:3]
#colnames(avg_tmp1)[1:3]<-c('Year', 'Month', 'low')

#saveRDS(avg_tmp, file='./avgtmp.RDS')
#saveRDS(avg_tmp1, file='./hightmp.RDS')
#saveRDS(avg_tmp, file='./lowtmp.RDS')



GDP <- read.csv("~/Desktop/shiny_app/data/GDP.csv")
tmp<-as.data.frame(melt(GDP, id.vars=c("Country.Name")))
colnames(tmp)<-c('Country', "Year", "GDP")
GDP_PC <- read.csv("~/Desktop/shiny_app/data/GDP_PC.csv")
tmp_pc<-as.data.frame(melt(GDP_PC, id.vars=c("Country.Name")))
colnames(tmp_pc)<-c('Country', "Year", "GDP_Per_Capita")
gdp_data<-inner_join(tmp, tmp_pc, by=c("Country", "Year")) %>%
  mutate(.,Year=as.integer(unlist(gsub('X', '',Year))))

#colnames(gdp_data[2])="Year"

###

#Creating the grand dataset!
grand_data<-inner_join(w, gdp_data, by=c("Country", 'Year'))
colnames(grand_data)<-c("Country", "Year", "CO2 emissions per capita based on production",
                        "CO2 embodied in domestic final demand", "CO2 embodied in final demand, shares by country of origin (emitter)",
                        "Demand-based CO2 productivity", "CO2 embodied in final demand per capita",
                        "Net-exports of CO2 emissions","CO2 emissions based on production",
                        "Production-based CO2 productivity","GDP","GDP_Per_Capita")
saveRDS(grand_data, file='./data/grand_data.RDS')



#####
#Picture
#
#Picture 1:
#http://www.treehugger.com/slideshows/clean-technology/7-terrifying-global-warming-pictures/
#
#Pic2:
#https://en.wikipedia.org/wiki/Global_warming#/media/File:World_map_showing_surface_temperature_trends_between_1950_and_2014.png
#
#Pic3:
#http://www.livescience.com/19212-sea-level-rise-ancient-future.html
#
#Pic4
#https://developmentwatchuk.wordpress.com/2011/02/18/where-should-you-live-to-avoid-global-warming/
#
#Pic5
#http://neo.sci.gsfc.nasa.gov/servlet/RenderData?si=1705120&cs=rgb&format=JPEG&width=3600&height=1800
