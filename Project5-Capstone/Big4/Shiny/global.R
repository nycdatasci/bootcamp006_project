# setwd("/Users/binfang/Documents/NYCDSA/project/Project_5/shiny_project")

require(leaflet)
require(shinythemes)
library(rgeos)
library(sp)
library(rgdal)
library(data.table)

# Base Map
setwd("/Users/binfang/Documents/NYCDSA/project/Project_5/shiny_project/nyc_shp")
nyczipcode <- readOGR("nyc.shp", layer = "nyc", verbose = FALSE)

# Data Table
path = "https://raw.githubusercontent.com/fangbin08/Data/master/shinydata.csv"
taxidata <- read.csv(path, sep = "\t")

####################################################################################
# Preprocess Taxi Data
variables = taxidata[,1:7]
variables = do.call("rbind", replicate(3, variables, simplify = F))


RFR_pred = data.frame(taxidata$RFR_pred)
XGB_pred = data.frame(taxidata$XGB_pred)
Ensemble_pred = data.frame(taxidata$Ensemble_pred)

colnames(RFR_pred) = "Count"
colnames(XGB_pred) = "Count"
colnames(Ensemble_pred) = "Count"
pred_count = rbind(RFR_pred,XGB_pred,Ensemble_pred)

type = data.frame(rep(c(1,2,3), each=33264))

taxidata = cbind(variables,pred_count,type)
names(taxidata)[names(taxidata) == colnames(taxidata[9])]  = 'Type'
rm(RFR_pred,XGB_pred,Ensemble_pred,variables,type,pred_count)



  
  # date_input <- as.numeric(which(numbers_date$date == as.character(input$slt_date)) -1)
  # count_tojoin = taxidata %>%
  #   filter(DayofWeek == "3") %>%
  #   # filter(Hour == "0") %>%
  #   group_by(Zipcode) %>%
  #   summarise(Total = sum(Count)) %>%
  #   mutate(Zipcode=factor(Zipcode))
  # 
  # # nyczipcode@data = merge(x = nyczipcode@data, y = count_tojoin,
  # #                         by.x='GEOID10', by.y='Zipcode', all.x = TRUE)
  # 
  # total <- count_tojoin$Total
  # names(total) <- count_tojoin$Zipcode
  # nyczipcode@data$Total <- total[as.character(nyczipcode@data$GEOID10)]
  


# Create date variable
numbers = c(0,1,2,3,4,5,6)
date = c("2016-09-19","2016-09-20","2016-09-21",
         "2016-09-22","2016-09-23","2016-09-24",
         "2016-09-25")
numbers_date = data.frame(cbind(numbers, date))




