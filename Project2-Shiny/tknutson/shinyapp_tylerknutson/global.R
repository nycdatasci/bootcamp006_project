library(shiny)
library(shinydashboard)
library(dplyr)
library(googleVis)
library(leaflet)
library(tidyr)
library(highcharter)
library(ggplot2)
library(ggthemes)
library(scales)
library(RColorBrewer)



# import csv files with agricultural (c_agro_all) and wine.com api (api_all) data; prep code to create these files in separate r file

api_all = read.csv("api_all.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
c_agro_all = read.csv("c_agro_all.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
varietal_xwalk = read.csv("agro_api_varietal_xwalk.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

# sorted varietal values, by wine type then alphaetical

varietal_df <- api_all %>% 
  select(appellation_name, wine_type_name, varietal) %>% 
  filter(wine_type_name %in% c("All Types", "Red Wines", "White Wines")) %>% 
  group_by(appellation_name, wine_type_name, varietal) %>%
  summarise() %>%
  select(varietal)


# max tons crushed

mtc = max(c_agro_all$tons_crushed)




#agro data has many to many mapping, so replacing some values accordingly





# old conditional axes (not used)
# vAxes =
#   ifelse("All Grapes" %in% input$varietalName,
#          "[{title:'Tons Crushed',
#                               format:'#,###K',
#                               textPosition: 'out',
#                               viewWindowMode:'explicit',
# 			                        viewWindow:{min:0, max:5000}}]"
#          ,
#          
#          "[{title:'Tons Crushed',
#                               format:'#,###K',
#                               textPosition: 'out',
#                               viewWindowMode:'explicit',
#                               viewWindow:{min:0, max:800}}]"
#   ),




