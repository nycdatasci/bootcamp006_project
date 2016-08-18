# install.packages("rCharts")
# install.packages("devtools")
# install_github("rCharts","ramnathv")

library(rCharts)
library(devtools)
library(stringr)
library(leaflet)
library(dplyr)
library(htmltools)
library(reshape2)
library(googleVis)
library(data.table)

# setwd("C:/Users/Samriddhi/Desktop/NYCDataScience/Python/Trulia")

ApartmentComplex = read.csv("./www/ApartmentsComplexes.csv")
IndividualApartment = read.csv("./www/Apartments.csv")

Central_Harlem = c(	10026, 10027, 10030, 10037, 10039)
Chelsea_Clinton	 = c(10001, 10011, 10018, 10019, 10020, 10036)
East_Harlem = c	(10029, 10035)
GramercyPark_MurrayHill = c(	10010, 10016, 10017, 10022)
GreenwichVillage_Soho =c (	10012, 10013, 10014)
Lower_Manhattan = c(	10004, 10005, 10006, 10007, 10038, 10280)
Lower_EastSide =c(	10002, 10003, 10009 )
Upper_EastSide =c (	10021, 10028, 10044, 10065, 10075, 10128)
Upper_WestSide	= c(10023, 10024, 10025)
Inwood_WashingtonHeights	= c(10031, 10032, 10033, 10034, 10040)


ApartmentComplex2 = ApartmentComplex %>%
  mutate(Neighborhood = ifelse(Postal %in%  Central_Harlem, "Central Harlem",
  ifelse(Postal %in%  Chelsea_Clinton, "Chelsea Clinton",
  ifelse(Postal %in%  East_Harlem, "East Harlem",
  ifelse(Postal %in%  GramercyPark_MurrayHill, "GramercyPark MurrayHill",
  ifelse(Postal %in%  GreenwichVillage_Soho, "GreenwichVillage Soho",
  ifelse(Postal %in%  Lower_Manhattan, "Lower Manhattan",
  ifelse(Postal %in%  Lower_EastSide, "Lower EastSide",
  ifelse(Postal %in%  Upper_EastSide, "Upper EastSide",
  ifelse(Postal %in%  Upper_WestSide, "Upper_WestSide",
  ifelse(Postal %in%  Inwood_WashingtonHeights, "Inwood WashingtonHeights",
  "Unknown" )))))))))))

ApartmentComplex2 = ApartmentComplex2[ApartmentComplex2$Neighborhood !="Unknown", ]

IndividualApartment2 = IndividualApartment %>%
  mutate(Neighborhood = ifelse(Postal %in%  Central_Harlem, "Central Harlem",
  ifelse(Postal %in%  Chelsea_Clinton, "Chelsea Clinton",
  ifelse(Postal %in%  East_Harlem, "East Harlem",
  ifelse(Postal %in%  GramercyPark_MurrayHill, "GramercyPark MurrayHill",
  ifelse(Postal %in%  GreenwichVillage_Soho, "GreenwichVillage Soho",
  ifelse(Postal %in%  Lower_Manhattan, "Lower Manhattan",
  ifelse(Postal %in%  Lower_EastSide, "Lower EastSide",
  ifelse(Postal %in%  Upper_EastSide, "Upper EastSide",
  ifelse(Postal %in%  Upper_WestSide, "Upper_WestSide",
  ifelse(Postal %in%  Inwood_WashingtonHeights, "Inwood WashingtonHeights",
  "Unknown" )))))))))))
IndividualApartment2 = IndividualApartment2[IndividualApartment2$Neighborhood !="Unknown", ]

IndividualApartment2$NoOfBedroom = gsub("bd","",IndividualApartment2$NoOfBedroom)
IndividualApartment2$NoOfBathRoom = gsub("ba","",IndividualApartment2$NoOfBathRoom)
IndividualApartment2$Area = gsub("sqft","",IndividualApartment2$Area)


IndividualApartment2$NoOfBedroom  = as.numeric(IndividualApartment2$NoOfBedroom)
IndividualApartment2$NoOfBathRoom  = as.numeric(IndividualApartment2$NoOfBathRoom)
IndividualApartment2$Area  = as.numeric(IndividualApartment2$Area)

IndividualApartment2$Neighborhood = as.factor(IndividualApartment2$Neighborhood)



IndividualApartment2 = IndividualApartment2 %>%
  mutate(Bedroom_number = 
                      ifelse(IndividualApartment2$NoOfBedroom == 1, "1 bed",
                      ifelse(IndividualApartment2$NoOfBedroom == 2, "2 bed",
                      ifelse(IndividualApartment2$NoOfBedroom == 3, "3 bed",
                      "3+ bed" ))))

IndividualApartment2 = IndividualApartment2 %>%
  mutate(Bedroom_number = ifelse(is.na(Bedroom_number),"Null",Bedroom_number))




#-----------------------------------------------------------------------------------------------------


Apt_Bedrooms_rent =IndividualApartment2 %>% 
  group_by(Bedroom_number) %>%
  summarise( TotalApartments = n(), AvgRent = median(RentRate) )



# 
# IndividualApartment2 %>% 
#   group_by(Neighborhood, Bedroom_number) %>%
#   summarise( TotalApartments = n(), AvgRent = median(RentRate) )

#-----------------------------------------------------------------------------------------------------

# Apartment No of Bedrrom, Rent price, Neighboorhood
Apt_Bedrooms_rent_Neighborhood =IndividualApartment2 %>% 
  group_by(Neighborhood, Bedroom_number) %>%
  summarise( TotalApartments = n(), MedianRent = median(RentRate) )


Apt_Bedrooms_rent_Neighborhood_2 = dcast(Apt_Bedrooms_rent_Neighborhood,
                                         Neighborhood ~ Bedroom_number, value.var= "TotalApartments")

Apt_Bedrooms_rent_Neighborhood_3 = dcast(Apt_Bedrooms_rent_Neighborhood,
                                         Neighborhood ~ Bedroom_number, value.var= "MedianRent")


#-----------------------------------------------------------------------------------------------------


Apt_Crime_Neighborhood =   IndividualApartment2 %>% 
  group_by(Neighborhood, CrimeLevel) %>%
  summarise( frequency = n() )


#dcast
Apt_Crime_Neighborhood_2 = dcast(Apt_Crime_Neighborhood,
                                 Neighborhood ~ CrimeLevel, value.var= "frequency")



#-----------------------------------------------------------------------------------------------------



Apt_ElementarySchool_Neighborhood =   IndividualApartment2 %>% 
  group_by(Neighborhood, ElementarySchool_rating) %>%
  summarise( frequency = n() )


#dcast
Apt_ElementarySchool_Neighborhood_2 = dcast(Apt_ElementarySchool_Neighborhood,
                                            Neighborhood ~ ElementarySchool_rating, value.var= "frequency")

names(Apt_ElementarySchool_Neighborhood_2)[5] = "Not Mentioned"
names(Apt_ElementarySchool_Neighborhood_2)


#------------------------------------------------------------------------------------------------------

Apt_MiddleSchool_Neighborhood =   IndividualApartment2 %>% 
  group_by(Neighborhood, MiddleSchool_rating) %>%
  summarise( frequency = n() )


#dcast
Apt_MiddleSchool_Neighborhood_2 = dcast(Apt_MiddleSchool_Neighborhood,
                                        Neighborhood ~ MiddleSchool_rating, value.var= "frequency")

names(Apt_MiddleSchool_Neighborhood_2)[5] = "Not Mentioned"
names(Apt_MiddleSchool_Neighborhood_2)


#--------------------------------------------------------------------------------------------------------


Apt_HighSchool_Neighborhood =   IndividualApartment2 %>% 
  group_by(Neighborhood, HighSchool_rating) %>%
  summarise( frequency = n() )


#dcast
Apt_HighSchool_Neighborhood_2 = dcast(Apt_HighSchool_Neighborhood,
                                      Neighborhood ~ HighSchool_rating, value.var= "frequency")

names(Apt_HighSchool_Neighborhood_2)[5] = "Not Mentioned"
names(Apt_HighSchool_Neighborhood_2)

#-----------------------------------------------------------------------------------------------------
