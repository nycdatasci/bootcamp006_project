library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(dygraphs)
library(xts)
library(DT)

# Function to get the first day of the month, given a date
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# Local path
# setwd("~/Documents/NYCDSA/Project 2")

# First dataset
balkanRoute = readRDS('data/PathGreeceBalkans2016.rds', refhook = 'Balkans')
mediteRoute = readRDS('data/PathSpainItaly2016.rds', refhook = 'Medite')
# Modify to join worldMap: Add country and coordinates:

# First, turn columns into country names, except Date
colnames(balkanRoute)[3:length(balkanRoute)] =
strsplit(names(balkanRoute)[3:length(balkanRoute)], 'to[.]') %>%
sapply(., function(x){x[length(x)]})
colnames(balkanRoute)[2] = c('Greece')

# Next, gather countries and add Geo-location. Used for Leaflet map
# geo_loc = geocode(names(balkanRoute)[2:length(balkanRoute)]) %>% 
# mutate(.,"Country" = names(balkanRoute)[2:length(balkanRoute)])

# Next load country codes and join them to the initial data frame. Note that 
# mediteRoute.map is already in months
country_codes =  readRDS('data/countryCodes.rds', refhook = 'codes')

# Aggregate by month and back to data frame using xts. Changing Date format
# in month year. gathering and joining with country codes
balkanTimeSeries = xts(balkanRoute[,-1] ,order.by = unique(balkanRoute$Date))

balkanTimeSeries_month = apply.monthly(balkanTimeSeries,FUN=colSums)

balkanRoute.map = data.frame(Date = index(balkanTimeSeries_month), 
                                        coredata(balkanTimeSeries_month)) %>% 
                  mutate(.,Date = monthStart(Date)) %>%
                  gather(.,key="Country",value="Arrivals",2:8) %>% 
                  inner_join(.,country_codes,by = "Country")

mediteRoute.map = mediteRoute %>% 
                  inner_join(.,country_codes,by = "Country") %>%
                  mutate(.,Date = monthStart(Date))

# Set input slider
slider.range = unique(balkanRoute.map$Date)

# Second dataset: About country of origin
dataOrigin2016 = readRDS('data/dataOrigin2016.rds', refhook = 'dataOrigin')

# Third dataset: About gender distribution
dataGender2016 = readRDS('data/dataGender2016.rds', refhook = 'dataGender')

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
