library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(dygraphs)
library(xts)
library(DT)
# library(leaflet)
# library(ggmap)

# Local path
setwd("~/Documents/NYCDSA/Project 2")

# First dataset
balkanRoute = readRDS('data/PathGreeceBalkans2016.rds', refhook = 'Balkans')

# Modify to join worldMap: Add country and coordinates:

# First, turn columns into country names, except Date
colnames(balkanRoute)[3:length(balkanRoute)] =
strsplit(names(balkanRoute)[3:length(balkanRoute)], 'to[.]') %>%
sapply(., function(x){x[length(x)]})
colnames(balkanRoute)[2] = c('Greece')

# Next, gather countries and add Geo-location. Used for Leaflet map
# geo_loc = geocode(names(balkanRoute)[2:length(balkanRoute)]) %>% 
# mutate(.,"Country" = names(balkanRoute)[2:length(balkanRoute)])

# Next load country codes and join them to the initial data frame
country_codes =  readRDS('data/countryCodes.rds', refhook = 'codes')

balkanRoute.map = balkanRoute %>% 
                  gather(.,key="Country",value="Arrivals",2:8) %>% 
                  inner_join(.,country_codes,by = "Country")

balkanTimeSeries = xts(balkanRoute[,-1],order.by = unique(balkanRoute$Date))
# Aggregate by month
TimeSeries_montly = apply.monthly(balkanTimeSeries,FUN=colSums)
# Append data for italy ??? 

slider.range = balkanRoute.map$Date

# Second dataset: About country of origin
dataOrigin2016 = readRDS('data/dataOrigin2016.rds', refhook = 'dataOrigin')

# Third dataset: About gender distribution
dataGender2016 = readRDS('data/dataGender2016.rds', refhook = 'dataGender')

# Fourth dataset: Only annual numbers, for the moment

otherMarkers2016 = data.frame('Country'=c('Italy','Spain','Turkey'),
                              'Code' = c('ITA', 'SPA','TUR'), 
                              'lon'=  c(12.5, -3, 32.50), 
                              'lat' = c(42, 40, 39.5),
                              'Total'=c(93611, 2476, 660754))
otherMarkers2016$hover <- paste("Arrivals Oct-15, May-16: ", otherMarkers2016$Total)

# Initialize tab and define choices for InputBoxes
# Inputbox "whichData"
availDatasets = c('Demography', 'Gender', 'Daily Arrivals')

# Inputbox "selected"
col_balkanRoute = colnames(balkanRoute)
col_demography = colnames(dataOrigin2016)
col_gender = colnames(dataGender2016)

# Inputbox "origin"
countries = c(unique(dataOrigin2016$Country),'Total')


col = col_demography
data_stat = dataOrigin2016
