if (!require(openxlsx)) install.packages(openxlsx)
if (!require(shinythemes)) install.packages(shinythemes)
if (!require(shiny)) install.packages(shiny)
if (!require(googleVis)) install.packages(googleVis)
if (!require(dplyr)) install.packages(dplyr)
if (!require(reshape2)) install.packages(reshape2)
library(openxlsx)
library(shiny)
library(shinythemes)
library(googleVis) 
library(dplyr)
library(reshape2)


# load data
marital_data = read.xlsx('data/marital_data.xlsx', colNames=T)

marital_total = read.xlsx('data/marital_total.xlsx', colNames=T)

gender_data = read.xlsx('data/gender_data.xlsx', colNames=T)

race_data = read.xlsx('data/race_data.xlsx', colNames=T)

race_gender_data = read.xlsx('data/race_gender_data.xlsx', colNames=T)

hist_data = read.xlsx('data/(master)Income_Marital.xlsx', colNames=T)

optional_columns = c('Marital_Status', 'Year', 'Gender', 'Race', 'Median', 'Standard_Error(Median)','Mean', 'Standard_Error(Mean)')

# define choices
marital_choice <- unique(marital_data$Marital_Status)
year_choice <-unique(hist_data$Year)
race_choice <- unique(race_data$Race)
gender_choice <- unique(gender_data$Gender)

points = 0
