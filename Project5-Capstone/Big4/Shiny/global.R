# setwd("/Users/binfang/Documents/NYCDSA/project/Project_5/shiny_project")

require(leaflet)
require(shinythemes)
library(rgeos)
library(sp)
library(rgdal)
library(data.table)

# Base Map
path1 = "./nyc_shp/nyc.shp"
# path1 = "https://github.com/fangbin08/Data/tree/master/nyc_shp/nyc.shp"
nyczipcode <- readOGR(path1, layer = "nyc", verbose = FALSE)

# Data Table
path2 = "https://raw.githubusercontent.com/fangbin08/Data/master/shinyData_update.csv"
taxidata <- read.csv(path2, sep = ",")

####################################################################################
# Preprocess Taxi Data
variables = taxidata[,1:6]
variables = do.call("rbind", replicate(3, variables, simplify = F))


RFR_pred = data.frame(taxidata[,7])
XGB_pred = data.frame(taxidata[,8])
Ensemble_pred = data.frame(taxidata[,9])

colnames(RFR_pred) = "Count"
colnames(XGB_pred) = "Count"
colnames(Ensemble_pred) = "Count"
pred_count = rbind(RFR_pred,XGB_pred,Ensemble_pred)

type = data.frame(rep(c(1,2,3), each=33432))

taxidata = cbind(variables,pred_count,type)
names(taxidata)[names(taxidata) == colnames(taxidata[8])]  = 'Type'
rm(RFR_pred,XGB_pred,Ensemble_pred,variables,type,pred_count)

taxidata$Type[taxidata$Type == 1] = "RF"
taxidata$Type[taxidata$Type == 2] = "XGB"
taxidata$Type[taxidata$Type == 3] = "ENS"

  
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
date = c("2016-09-22", "2016-09-23","2016-09-24","2016-09-25",
         "2016-09-26","2016-09-27","2016-09-28")
numbers_date = data.frame(cbind(numbers, date))

# Edit Popup
# count_popup <- paste0(nyczipcode@data$)

