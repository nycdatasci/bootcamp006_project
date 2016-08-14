library(shiny)
library(dplyr)
library(dtplyr) # Make dplyr output as data.table
library(data.table)
library(DT)
library(ggplot2)

load('Data/data.RData')

# Separate this from food for better performance
food_reactive <- reactive(food)

# -----------------------------------------------------------------

#### Options for Region Mapping Tab ####
color_axis = "{
                 values:[300, 3000, 6000],
                 colors:['#ABC8E2', '#375D81', '#183152']
                }"

region_geo <- data.frame(
  Region = c('World','Northern Europe', 
             'Western Europe','Southern Europe'),
  GeoCode = c('world', '154', '155', '039'),
  stringsAsFactors = F
)

# -----------------------------------------------------------------

#### Variables for Product Tab ####
# Filter Box
nutritions <- names(food)[grepl('*100g', names(food))] 

rangs <- unlist(sapply(nutritions, 
                       function(c) {
                         range(food[,c,with = F], na.rm = T)
                         }))

min_v <- rangs[1,] %>% pmax(0)
max_v <- rangs[2,]

# Function to generate buttons that linked to food fact website
createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">Info</a>',val)
}

# -----------------------------------------------------------------