# Source data files

library(choroplethr)
library(choroplethrMaps)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)



# Import data
crime <- read.csv('D:/Projects/DataScienceBootcamp/11_Project1_Exploratory_Data_Analysis/01_External/02_Data.ny.gov/Index__Violent__Property__and_Firearm_Rates_By_County__Beginning_1990.csv')

# Clean data
crime$County[crime$County=='St Lawrence'] <- 'St. Lawrence'
crime$County <- droplevels(crime$County)

# Add area data
# https://en.wikipedia.org/wiki/List_of_counties_in_New_York
sq.mi <- c(533,1034,57.43,715,1310,864,1500,410.81,898.85,1118,648,502,1468,825,
           1227,1916,1697,533,495,658,1808,1458,1857,96.9,1290,640,662,1366,410,
           453,33.77,1140,1213,806,662,839,817,1312,1003,246,178.28,665,102.5,
           199,2821,844,210,626,342,325,1404,2373,997,523,476,1161,870,846,1384,
           500,596,376)
sq.km <- c(1380,2678,149,1852,3393,2238,3885,1064,2328,2896,1678,1300,3802,2137,
           3178,4962,4395,1380,1282,1704,4683,3776,4810,251,3341,1658,1715,3538,
           1062,1173,87,2953,3142,2088,1715,2173,2116,3398,2598,637,462,1722,
           265,515,7306,2186,544,1621,886,842,3636,6146,2582,1355,1233,3007,
           2253,2191,3585,1295,1544,974)
area <- data.frame('County' = levels(crime$County), 'Area.sq.mi' = sq.mi,
                   'Area.sq.km' = sq.km); rm(sq.mi, sq.km)
crime <- merge(crime, area); rm(area)
crime$Density.sq.mi <- crime$Population/crime$Area.sq.mi
crime$Density.sq.km <- crime$Population/crime$Area.sq.km
attach(crime)

# Split NYC and rest - NYC data only between 1990-2001
crime_nys <- subset(crime, County!='Bronx' & 
                        County!='Kings' & 
                        County!='New York' & 
                        County!='Queens' & 
                        County!='Richmond')
crime_nyc <- subset(crime, County=='Bronx' | 
                        County=='Kings' |
                        County=='New York' | 
                        County=='Queens' | 
                        County=='Richmond')

data(df_pop_county)
ny_choro <- df_pop_county[1829:1890,]
ny_choro$value <- log(subset(crime$Density.sq.mi, Year==2000))

ny_counties <- df_pop_county[1829:1890,]
nyc_counties <- c(36005,36047,36061,36081,36085)
nys_counties <- ny_counties[ny_counties$region!=nyc_counties,]


u500 <- crime %>% filter(Density.sq.mi<500)
oe500 <- crime %>% filter(Density.sq.mi>=500)
u500nys <- crime_nys %>% filter(Density.sq.mi<500)
oe500nys <- crime_nys %>% filter(Density.sq.mi>=500)
oe500nyc <- crime_nyc %>% filter(Density.sq.mi>=500)

