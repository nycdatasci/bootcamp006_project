library(shiny)
library(plotly)
library(dplyr)
library(countrycode)

mug <- read.csv("https://raw.githubusercontent.com/amy17519/FredorangeMuggers/master/FredorangeMug.csv")
user <- read.csv("https://raw.githubusercontent.com/amy17519/FredorangeMuggers/master/FredorangeUser.csv")


mug_country<-as.data.frame(mug %>% 
                             group_by(Country) %>%
                             summarise(Quantity = length(Country)))
user_country<-as.data.frame(user %>% 
                              group_by(Country) %>%
                              summarise(Quantity = length(Country)))

#take out users who did not indicate country. There are 1646 of them.
user_country<-user_country[-which(user_country$Country=='undefined country'),]

mug_country$CountryCode<-countrycode(mug_country$Country,"country.name", "iso3c")
user_country$CountryCode<-countrycode(user_country$Country,"country.name", "iso3c")

mug_country_noUSA<-mug_country[-which(mug_country$Country=='USA'),]
user_country_noUSA<-user_country[-which(user_country$Country=='usa'),]

#plotly map item
l <- list(color = toRGB("gray"), width = 0.5)
g <- list(showframe = FALSE,showcoastlines = FALSE,
          projection = list(type ='equirectangular'))


set.seed(123)
mugCluster <- kmeans(mug[, 7:9], 4, nstart = 20)

#observe patterns in each cluster, rename clusters by their characteristics, then #use it as a new variable: Difficulty
mugCluster$centers
mugCluster$cluster <- as.factor(mugCluster$cluster)
levels(mugCluster$cluster) <- c("Medium Difficulty","Inconclusive",
                                "Hard to Get Mugs","Easy to Get Mugs")
mug$Difficulty <- mugCluster$cluster


popular_editions<-mug[mug$Edition %in% c("08 Icon Edition","13 You Are Here Series",
                                         "Japan Country Series",'Relief Series'),]
popular_editions$Edition<-as.factor(as.character(popular_editions$Edition))


popular_country<-mug[mug$Country %in%
                       c("Canada","China","Germany","Mexico","Russia","USA"),]
popular_country$Country<-as.factor(as.character(popular_country$Country))