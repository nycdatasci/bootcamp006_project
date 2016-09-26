library(stringr)
library(leaflet)
library(dplyr)
library(htmltools)
library(reshape2)
library(googleVis)
library(data.table)
library(ggvis)
library(LDAvis)
library(DT)
library(shinydashboard)



yelpBusiness= read.csv("./www/Biz_NoReview_latlong_09172016.csv",stringsAsFactors = F)
yelpBusiness =   yelpBusiness[, -1 ]
yelpBusiness =   yelpBusiness %>%
  mutate(NewRating = ifelse(stars_of_business >=  AVG_stars_category_round ,
                              "Above Average" ,"Below Average"))

# removing category with burgers and Restaurants
yelpBusiness1 = subset(yelpBusiness, categories != "Burgers" )
yelpBizCat= yelpBusiness1 %>%
  group_by(categories) %>%
  summarise(countTotal = n())
yelpBizCat =  yelpBizCat[order(-yelpBizCat$countTotal),]
yelpBizCat = subset(yelpBizCat,categories != "['Restaurants']")


# Getting only top 20 categories and naming others as "others"
top20cat = head(yelpBizCat$categories,20)
yelpBusiness1 =   yelpBusiness1 %>%
  mutate(NewCategory = ifelse(yelpBusiness1$categories %in%  top20cat ,
                              yelpBusiness1$categories ,"Others"))

yelpBusiness1$PriceRange[is.na(yelpBusiness1$PriceRange)] ="Not Given"

yelpBusiness1 =   yelpBusiness1 %>%
  mutate(PriceCategory = ifelse(yelpBusiness1$PriceRange == "1" , "1.Cheap - ($)",
                         ifelse(yelpBusiness1$PriceRange == "2" , "2.Average - ($$)",
                         ifelse(yelpBusiness1$PriceRange == "3" , "3.Expensive - ($$$)",
                         ifelse(yelpBusiness1$PriceRange == "4" , "4.Very Expensive - ($$$$)",
                          "5.Not Given")))))


####################################checking time###########################


checkingTime= read.csv("./www/checkingtime.csv")
names(checkingTime)[3:4] <- c("days","hours")

checkingtime_days = checkingTime %>%
  group_by(days) %>%
  summarise(checkingTotal = sum(check_in_number))

checkingtime_time = checkingTime %>%
  group_by(hours) %>%
  summarise(checkingTotal = sum(check_in_number)) 

stringToTime = function(s){
  as.integer(gsub(pattern = ':', replacement = '', x = substr(s, 1, 2)))
}

checkingtime_time['hours_int'] = sapply(checkingtime_time$hours, stringToTime)
checkingtime_time = arrange(checkingtime_time, hours_int)


#days
checkingtime_days = checkingTime %>%
  group_by(days) %>%
  summarise(checkingTotal = sum(check_in_number))
checkingtime_days['dayindex'] = c(6,2,7,1,5,3,4)
checkingtime_days = arrange(checkingtime_days, dayindex)

#dayhours
checkingtime_dayhours = checkingTime %>%
  arrange(desc(check_in_number)) %>%
  top_n(15)
checkingtime_dayhours['dayHour'] = paste(checkingtime_dayhours$days, checkingtime_dayhours$hours)
checkingtime_dayhours$dayHour <- factor(checkingtime_dayhours$dayHour, 
                                        levels=unique(checkingtime_dayhours$dayHour))

######################################### Loading LDA json files Rdata ###############################

#Loading Top 5 common LDAs
load('./www/American_LDA.Rdata')
load('./www/Thai_LDA.Rdata')
load('./www/Mexican_LDA.Rdata')
load('./www/Italian_LDA.Rdata')
load('./www/Chinese_LDA.Rdata')














