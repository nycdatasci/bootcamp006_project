library(plyr)
library(dplyr)
library(shiny)
library(googleVis)
library(datasets)
library(shinythemes)
library(dygraphs)
library(Hmisc)


z = read.csv('foo.csv', stringsAsFactors = F)
#z = kNN(u, k = 10) #Imputing using 5NN.


for (i in 2:6){
  z[,i] <- sapply(z[, i], function(x) as.numeric(gsub("[,$]", "", x)))
}

z$Date <- as.Date(z$Date, "%m/%d/%Y")


top <- read.csv('top_20_business_vs_pleasure.csv', stringsAsFactors = F)

x <- top
x$Business.Travel.Percent.of.Total <- as.numeric(sub("%", "", x$Business.Travel.Percent.of.Total))
x$Pleasure.Travel.Percent.of.Total <- as.numeric(sub("%", "", x$Pleasure.Travel.Percent.of.Total))
x$Total.Arrivals <- as.numeric( sub(",", "", x$Total.Arrivals ) )
x <- x[,c(1,5,8,9)]
x$business <- x$Total.Arrivals * (x$Business.Travel.Percent.of.Total / 100)
x$pleasure <- x$Total.Arrivals * (x$Pleasure.Travel.Percent.of.Total / 100)
p2 <- x[,c(1,5,6)]


top <- top[,c(1,5)]
names(top)[1] <- 'country'
names(top)[2] <- 'value'
top$value <- as.numeric( sub(",", "", top$value ) )

nam <- read.csv('north_america.csv', stringsAsFactors = F)
nam <- nam[,c(1,2)]
names(nam)[1] <- 'country'
names(nam)[2] <- 'value'
nam$value <- as.numeric( sub(",", "", nam$value) )


world <- read.csv('world_regions.csv', stringsAsFactors = F)
names(world)

names(world)[1] <- 'Region'
names(world)[2] <- 'Arrival'
world$Arrival <- as.numeric( sub(",", "", world$Arrival) )
world <- world[,c(1,2)]
head(world)


topports <- read.csv('topports.csv', stringsAsFactors = F)
names(topports)
topports1 <- topports[,c(1,2)]
topports1$OVERSEAS.TOTAL.2016 <- as.numeric( sub(",", "", topports1$OVERSEAS.TOTAL.2016 ) )


