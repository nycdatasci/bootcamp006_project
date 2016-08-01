#install.packages('maps')
#install.packages('mapproj')
#install.packages('maptools')
#install.packages('ggmap')
#install.packages('dplyr')
#install.packages('shiny')
#install.packages('googleVis')
#install.packages('lazyeval')
#install.packages('grid')
#install.packages('gridExtra')
#install.packages('plotly')
#install.packages('shinydashboard')
#install.packages('RColorBrewer')
# install.packages('tidyr')
#install.packages("dygraphs")
#install.packages('reshape2")


options(rgl.useNULL=TRUE)
suppressWarnings(library(maps))
suppressWarnings(library(mapproj))
suppressWarnings(library(maptools))
suppressWarnings(library(ggmap))
suppressWarnings(library(ggplot2))
suppressWarnings(library(dplyr))
suppressWarnings(library(shiny))
suppressPackageStartupMessages(library(googleVis))
suppressWarnings(library(lazyeval))
suppressWarnings(library(grid))
suppressWarnings(library(gridExtra))
suppressWarnings(library(plotly))
suppressWarnings(library(shinydashboard))
suppressWarnings(library(RColorBrewer))
library(tidyr)
library(quantmod)
library(dygraphs)
library(shinyRGL)
library(rglwidget)
library(rgl)
library(reshape2)

# setwd("~/Desktop/shiny_app")


worldmap <- map_data('world')
worldmap <- worldmap[worldmap$region != "Antarctica",]

#load dataset
ghg<-readRDS("./GHG.RDS")
avg <- read.table("./data/avg_tmp.txt", quote="\"")[,c(1,3)]

grand_data<-readRDS("./data/grand_data.RDS")

#####Page1:
WorldMapViz <- function(df) { # optional input: qinfo
  
  g <- ggplot() + 
    geom_map(data = worldmap, map = worldmap, aes(map_id = region), 
             colour = "gray10", fill = "white", size = 0.1) +
    geom_map(data = df, map = worldmap, 
             aes(map_id = Country, fill = Value)) +
    expand_limits(x = worldmap$long, y = worldmap$lat) + 
    scale_fill_gradientn(
      guide = "colourbar",
      limits = c(min(df$Value), max(df$Value)), 
      colours = brewer.pal(11, name = "RdYlBu")
    ) +
    coord_fixed() +
    theme_nothing(legend = TRUE) +
    theme(
      panel.border = element_rect(fill = NA),
      legend.position = "bottom", legend.box = "horizontal",
      legend.key.width = unit(4, "cm")
    )
  
  g
}
