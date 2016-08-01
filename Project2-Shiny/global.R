if (!require(openxlsx))
  install.packages("openxlsx")
if (!require(shinythemes)) install.packages('shinythemes')
if (!require(DT)) install.packages('DT')
library(openxlsx)
library(shiny)
library(shinythemes)
library(DT)
library(googleVis) 
library(dplyr)

#load data
marital_data = read.xlsx('data/motion_chart_data.xlsx', colNames=T)

gender_data = read.xlsx('data/gender_data.xlsx', colNames=T)

race_data = read.xlsx('data/race_data.xlsx', colNames=T)

race_gender_data = read.xlsx('data/race_gender_data.xlsx', colNames=T)

hist_data = read.xlsx('data/(master)Income_Marital.xlsx', colNames=T)

optional_columns = c('Marital_Status', 'Year', 'Gender', 'Race', 'Median', 'Standard_Error(Median)',
                     'Mean', 'Standard_Error(Mean)')


