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
                                        'integer', 
                                        'integer', 
                                        'character', 
                                        'integer')) 
  {
  return(read.csv(filename, header = header, colClasses = colClasses))
}

without_duplicates = my_read_csv('Data/Non_duplicates.csv')

with_duplicates = my_read_csv('Data/Deal_data2.csv')





