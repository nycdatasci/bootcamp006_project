if (!require(shinythemes)) install.packages('shinythemes')
if (!require(shiny)) install.packages('shiny')
if (!require(googleVis)) install.packages('googleVis')
if (!require(dplyr)) install.packages('dplyr')
if (!require(reshape2)) install.packages('reshape2')
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(devtools)) install.packages('devtools')
if (!require(shinysky)) devtools::install_github("AnalytixWare/ShinySky")
if (!require(lubridate)) install.packages('lubridate')
library(shiny)
library(shinythemes)
library(shinysky)
library(googleVis)
library(reshape2)
library(dplyr)
require(lubridate)


#--------------------------#
#      load data
#--------------------------#
my_read_csv = function (filename, 
                         header = T, 
                         colClasses = c('character', 
                                        'character', 
                                        'character', 
                                        'Date', 
                                        'integer', 
                                        'integer', 
                                        'character', 
                                        'integer')) 
  {
  return(read.csv(filename, header = header, colClasses = colClasses))
}

without_duplicates = my_read_csv('Data/Without_duplicates.csv')

#--------------------------#
#       Compute data
#--------------------------#
popularity_overall = without_duplicates %>% group_by(Category, Store) %>% dplyr::summarise(NumDeals = n(), Popularity = sum(Popularity))
popularity_by_category = without_duplicates %>% group_by(Category) %>% dplyr::summarise(Total = sum(Popularity))
master_popularity_data = merge(popularity_overall, popularity_by_category, by = 'Category')
master_popularity_data$PPD = master_popularity_data$Popularity / master_popularity_data$NumDeals
most_store = master_popularity_data %>% select(Category, Store, NumberOfDeals = NumDeals)
most_store_temp = most_store %>% group_by(Category) %>% top_n(10, NumberOfDeals)


#--------------------------#
#   SelecInput choices  
#--------------------------#
category_choices = unique(without_duplicates$Category)
metric_choices = c('Both', 'By Comments', 'By Bookmarks')
minimum_choices = c(1, 5, 10, 20, 50)
store_names = unique(most_store_temp$Store)


