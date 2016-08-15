<<<<<<< HEAD
if (!require(shinythemes)) install.packages(shinythemes)
if (!require(shiny)) install.packages(shiny)
if (!require(googleVis)) install.packages(googleVis)
if (!require(dplyr)) install.packages(dplyr)
if (!require(reshape2)) install.packages(reshape2)
library(shiny)
library(shinythemes)
library(googleVis) 
library(dplyr)
library(reshape2)


#------------- load data
my_read_csv2 = function (filename, 
                         header = T, 
                         colClasses = c('factor', 
                                        'character', 
                                        'character', 
                                        'integer', 
=======
if (!require(shinythemes)) install.packages('shinythemes')
if (!require(shiny)) install.packages('shiny')
if (!require(googleVis)) install.packages('googleVis')
if (!require(dplyr)) install.packages('dplyr')
if (!require(reshape2)) install.packages('reshape2')
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(devtools)) install.packages('devtools')
if (!require(shinysky)) devtools::install_github("AnalytixWare/ShinySky")
library(shiny)
library(shinythemes)
library(shinysky)
library(googleVis)
library(reshape2)
library(dplyr)


#------------- load data
my_read_csv = function (filename, 
                         header = T, 
                         colClasses = c('character', 
                                        'character', 
                                        'character', 
                                        'Date', 
>>>>>>> d3eae80b1667a0750991df3a7dcd9151a194eac1
                                        'integer', 
                                        'integer', 
                                        'character', 
                                        'integer')) 
  {
  return(read.csv(filename, header = header, colClasses = colClasses))
}

<<<<<<< HEAD
without_duplicates = my_read_csv('Data/Non_duplicates.csv')

with_duplicates = my_read_csv('Data/Deal_data2.csv')




=======
without_duplicates = my_read_csv('Data/Without_duplicates.csv')
with_duplicates = my_read_csv('Data/Deal_data.csv')

popularity_overall = without_duplicates %>% group_by(Category, Store) %>% summarise(NumDeals = n(), Popularity = sum(Popularity))
popularity_by_category = without_duplicates %>% group_by(Category) %>% summarise(Total = sum(Popularity))
master_popularity_data = merge(popularity_overall, popularity_by_category, by = 'Category')
master_popularity_data$PPD = master_popularity_data$Popularity / master_popularity_data$NumDeals

most_store = master_popularity_data %>% group_by(Category) %>% select(Category, Store, NumberOfDeals = NumDeals) %>% top_n(10, NumberOfDeals)


category_choices = unique(without_duplicates$Category)
metric_choices = c('Both', 'Comments', 'Bookmarks')
minimum_choices = c(1, 5, 10, 20, 50)
store_names = unique(most_store$Store)
>>>>>>> d3eae80b1667a0750991df3a7dcd9151a194eac1

