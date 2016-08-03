#LOAD REQUIRED LIBRARIES
library (ggplot2) 
library (ggthemes) 
#library(zipcode) 
library (lubridate) 
library(labeling)
library(chron)
library(RColorBrewer)
library(plotly)
library(maps)
library (dplyr) 
library(stringr)
library(scales)
library(shiny)
library(googleVis)
library(shinydashboard)
library(shinythemes)
library(dygraphs)
library(Hmisc)
library(reshape2)

#READ DATA
# setwd("C:/Users/trichna/Documents/NYCDSA/Shiny")
#consumer_complaints = read.csv("C:/Users/trichna/Documents/NYCDSA/Shiny/Consumer_Complaints.csv",stringsAsFactors = FALSE)
consumer_complaints = read.csv('./data/Consumer_Complaints.csv', stringsAsFactors = F)
#consumer_complaints   = consumer_complaints  %>% dplyr::rename( Timely.response = Timely.response., Consumer.disputed = Consumer.disputed. )   %>% select(Product, Sub.product, Issue, Company, State, ZIP.code, Date.sent.to.company, Company.response.to.consumer, Timely.response, Consumer.disputed, Complaint.ID)
#consumer_complaints$Complaint_date = as.Date(consumer_complaints$ Date.sent.to.company,'%m/%d/%Y')
#consumer_complaints$Year = year(consumer_complaints$Complaint_date)

#consumer_compl =  consumer_complaints %>% filter(!Year %in% c(2011,2016)  )
#consumer_compl_prod = consumer_compl %>% filter(Product %in%  c("Consumer Loan",   "Bank account or service","Mortgage" , "Debt collection"  , "Credit card", "Credit reporting" , "Student loan","Money transfers","Prepaid card"   )  )
#consumer_compl_prod = consumer_compl_prod %>% filter(!State %in% c("PR", "AE", "MH", "AP", "FM", "VI", "GU", "MP", "AS", "PW", "AA",""))

########## Load data sets ##########
# consumer_compl_by_prod_year <- readRDS("./data/consumer_compl_by_prod_year.rds")
consumer_compl_by_prod_year <- readRDS("./data/consumer_compl_by_prod_year.rds")
consumer_compl_by_prod_issue <- readRDS("./data/consumer_compl_by_prod_issue.rds")
top25_institution_data_merged_disp_desc <- readRDS("./data/top25_institution_data_merged_disp_desc.rds")
top25_institution_data_merged_sla_desc <- readRDS("./data/top25_institution_data_merged_sla_desc.rds")
consumer_disp_map_merged <- readRDS("./data/consumer_disp_map_merged.rds")
consumer_compl_map_merged_sla <- readRDS("./data/consumer_compl_map_merged_sla.rds")
reshaped_late_response <- readRDS("./data/reshaped_late_response.rds")
reshaped_disputed_response <- readRDS("./data/reshaped_disputed_response.rds")
reshaped_monetary_relief <- readRDS("./data/reshaped_monetary_relief.rds")



graph1_cols =  c("Consumer Loan",   "Bank account or service","Mortgage" , "Debt collection"  , "Credit card", "Credit reporting" , "Student loan","Money transfers"  )

########## Define variables to be used in lists for choices ##########
productlist = c(
  "Bank account or service" ,
  "Consumer Loan" , 
  "Credit card" ,
  "Credit reporting" ,
  "Debt collection" ,
  "Money transfers",
  "Mortgage" ,
  "Student loan" 
)

yearlist = c(
2012, 2013, 2014, 2015  
)

tierlist = c("Tier 1",
"Tier 2",  
"Tier 3"
)

metriclist = c("% Delayed Responses", "% Disputed Responses")

metricmaplist = c("Volume of Complaints", "% Delayed Responses", "% Disputed Responses" )

metricinsightlist = c("% Delayed Responses", "% Disputed Responses", "% Cases closed with Monetary Relief")