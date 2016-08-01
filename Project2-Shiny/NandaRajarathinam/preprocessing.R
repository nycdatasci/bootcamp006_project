#LOAD REQUIRED LIBRARIES
library (ggplot2) 
library (ggthemes) 
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
library(datasets)
library(corrplot)

#READ DATA
setwd("C:\\Users\\trichna\\Documents\\NYCDSA\\Shiny")
#consumer_complaints = read.csv("C:\\Users\\trichna\\Documents\\NYCDSA\\Shiny\\Consumer_Complaints.csv",stringsAsFactors = FALSE)

c1 = c("Consumer Loan",   "Bank account or service","Mortgage" , "Debt collection"  , "Credit card", "Credit reporting" , "Student loan","Money transfers"  )
consumer_complaints = read.csv('Consumer_Complaints.csv', stringsAsFactors = F)
us_state_stat = read.csv('states_pop.csv',stringsAsFactors = F)
consumer_complaints   = consumer_complaints  %>% dplyr::rename( Timely.response = Timely.response., Consumer.disputed = Consumer.disputed. )   %>% select(Product, Sub.product, Issue, Company, State, ZIP.code, Date.sent.to.company, Company.response.to.consumer, Timely.response, Consumer.disputed, Complaint.ID)
consumer_complaints$Complaint_date = as.Date(consumer_complaints$ Date.sent.to.company,'%m/%d/%Y')
consumer_complaints$Year = as.character(year(consumer_complaints$Complaint_date))

consumer_compl =  consumer_complaints %>% filter(!Year %in% c(2011,2016)  )
consumer_compl_prod = consumer_compl %>% filter(Product %in%  c1  )
consumer_compl_prod = consumer_compl_prod %>% filter(!State %in% c("PR", "AE", "MH", "AP", "FM", "VI", "GU", "MP", "AS", "PW", "AA",""))
consumer_compl_prod = consumer_compl_prod %>% filter(Consumer.disputed != "" ) 


#Adding Resolution Type column
consumer_compl_prod[consumer_compl_prod$Company.response.to.consumer == "Closed with explanation", ]$Resolution_Type = "Closed without relief"
consumer_compl_prod[consumer_compl_prod$Company.response.to.consumer == "Closed without relief", ]$Resolution_Type = "Closed without relief"
consumer_compl_prod[consumer_compl_prod$Company.response.to.consumer == "Closed", ]$Resolution_Type = "Closed without relief"

consumer_compl_prod[consumer_compl_prod$Company.response.to.consumer == "Closed with monetary relief", ]$Resolution_Type = "Closed with relief"
consumer_compl_prod[consumer_compl_prod$Company.response.to.consumer == "Closed with non-monetary relief", ]$Resolution_Type = "Closed with relief"
consumer_compl_prod[consumer_compl_prod$Company.response.to.consumer == "Closed with relief", ]$Resolution_Type = "Closed with relief"

  
  
saveRDS(consumer_compl_prod,".\\Shinyproject\\data\\consumer_compl_prod.rds")

consumer_compl_by_prod_year = consumer_compl_prod %>%  group_by(Product,Year) %>%  dplyr::summarise(  cnt_complaints = n())
rr = reshape2::dcast(consumer_compl_by_prod_year,Year ~ Product, value.var = "cnt_complaints" ) 
saveRDS(rr,".\\Shinyproject\\data\\consumer_compl_by_prod_year.rds")


consumer_compl_by_prod_issue = consumer_compl_prod %>%  group_by(Product,Year,Issue) %>%  dplyr::summarize(  cnt_complaints = n())  %>%  do(head(., n = 5))
consumer_compl_by_prod_issue$Issue_clean = gsub(",",", ",consumer_compl_by_prod_issue$Issue)
consumer_compl_by_prod_issue = transform(consumer_compl_by_prod_issue, Issue_clean = reorder(Issue_clean,-cnt_complaints))
#reshaped_ds2 = reshape2::dcast(consumer_compl_by_prod_issue,Year ~ Issue_clean, value.var = "cnt_complaints" ) 
reshaped_ds2 =  consumer_compl_by_prod_issue
saveRDS(reshaped_ds2,".\\Shinyproject\\data\\consumer_compl_by_prod_issue.rds")

#------------------------------------------------------------------------------------------------
#Top 25 institutions based on # of issues (across years)
institution_compl_cnt = consumer_compl_prod  %>%  group_by(Company) %>%  dplyr::summarise(iss_cnt = n())  %>% arrange(desc(iss_cnt))  %>%  do(head(.,n = 25))
top25_institution_data = merge(institution_compl_cnt, consumer_compl_prod, by ='Company')
#considering only top 25 due to the volume of number of companies in the data. 
#Top 25 companies account for nearly 60% of the complaint whereas the other 3300 account for the remaining 40% of complaints
#Add a new variable called Company_Category to reflect the Tier of the comapnies based on number of complaints
top25_institution_data = top25_institution_data %>% mutate(Tier = ifelse(iss_cnt > 20000, 'Tier 1', ifelse(iss_cnt >5000, 'Tier 2','Tier 3')))   %>% filter(Consumer.disputed != "")

#Prepare dataset for graphing Consumer Dispute 
consumer_compl_prod_tier_grp = top25_institution_data %>%  group_by(Product,Tier,Year,Company) %>%  dplyr::summarize(  cnt_complaints = n())
consumer_compl_prod_dispute_grp = top25_institution_data %>%  group_by(Product,Tier,Year,Company, Consumer.disputed) %>%  dplyr::summarize(  cons_disp_cnt = n())

top25_institution_data_merged_disp = merge(consumer_compl_prod_tier_grp,consumer_compl_prod_dispute_grp, by = c('Product','Tier','Year','Company') )
top25_institution_data_merged_disp = top25_institution_data_merged_disp %>% filter(Consumer.disputed == 'Yes') %>% mutate(dispute_ratio = round(cons_disp_cnt / cnt_complaints,4))
top25_institution_data_merged_disp_desc = top25_institution_data_merged_disp %>% group_by(Product,Tier,Year,Company) %>%   arrange(Product,Tier,Year,Company,-dispute_ratio)

saveRDS(top25_institution_data_merged_disp_desc,".\\Shinyproject\\data\\top25_institution_data_merged_disp_desc.rds")

#Prepare dataset for graphing UnTimely response (missed SLA)
#consumer_compl_prod_tier_grp = top25_institution_data %>%  group_by(Product,Tier,Company) %>%  dplyr::summarize(  cnt_complaints = n())
consumer_compl_prod_sla_grp = top25_institution_data %>%  group_by(Product,Tier,Year,Company,Timely.response ) %>%  dplyr::summarize(  cons_sla_cnt = n())

top25_institution_data_merged_sla = merge(consumer_compl_prod_tier_grp,consumer_compl_prod_sla_grp, by = c('Product','Tier','Year','Company') )
top25_institution_data_merged_sla = top25_institution_data_merged_sla %>% filter(Timely.response == 'No') %>% mutate(missed_sla_ratio = round(cons_sla_cnt / cnt_complaints,4))
top25_institution_data_merged_sla_desc = top25_institution_data_merged_sla %>% group_by(Product,Tier,Year,Company) %>%   arrange(Product,Tier,Year,Company,-missed_sla_ratio)

saveRDS(top25_institution_data_merged_sla_desc,".\\Shinyproject\\data\\top25_institution_data_merged_sla_desc.rds")


#Prepare dataset for graphing the Map
require(datasets)
st = data.frame(state.abb,state.name,state.region)

consumer_compl_map = consumer_compl_prod %>% filter(Consumer.disputed != "") 
consumer_compl_map_merged = merge(consumer_compl_map, st, by.x = "State", by.y = "state.abb" )
consumer_compl_map_merged_grp = consumer_compl_map_merged %>%  group_by(Product,Year,state.name) %>%  dplyr::summarize(  cnt_complaints = n())
consumer_compl_map_dispute_grp = consumer_compl_map_merged %>%  group_by(Product,Year,state.name, Consumer.disputed) %>%  dplyr::summarize(  cnt_disp_complaints = n())

consumer_disp_map_merged = merge(consumer_compl_map_merged_grp, consumer_compl_map_dispute_grp, by =c('Product','Year','state.name'))
consumer_disp_map_merged = consumer_disp_map_merged %>%  dplyr::mutate(bool = Consumer.disputed =='Yes') %>%  filter(bool == TRUE)        %>%   dplyr::mutate(dispute_ratio = round((cnt_disp_complaints / cnt_complaints)*100, digits = 4 ))

saveRDS(consumer_disp_map_merged,".\\Shinyproject\\data\\consumer_disp_map_merged.rds")

#calculate % of Monetary Relief
#consumer_compl_map_monetary = consumer_compl_map_merged %>% group_by(Product,Year,state.name, Company.response.to.consumer) %>% summarise(response_count = n())  
#consumer_compl_map_monetary_tot = consumer_compl_map_monetary %>% summarise(complaints_total = sum(response_count) )
#consumer_compl_map_monetary_merged = merge(consumer_compl_map_monetary,consumer_compl_map_monetary_tot, by = c('Product','Year','state.name') )

#consumer_compl_map_monetary_fil_merged = consumer_compl_map_monetary_merged %>%  filter(Company.response.to.consumer == 'Closed with monetary relief')       %>% mutate(response_ratio = response_count / complaints_total)

#--View(consumer_compl_map_monetary_fil_merged)
consumer_compl_map_sla = consumer_compl_map_merged %>%  group_by(Product,Year,state.name) %>%  dplyr::summarize(  cnt_complaints = n())
consumer_compl_map_sla_miss = consumer_compl_map_merged %>% group_by(Product,Year,state.name,Timely.response ) %>% summarise(timely_response_count = n())  
consumer_compl_map_merged_sla = merge(consumer_compl_map_sla,consumer_compl_map_sla_miss, by = c('Product','Year','state.name') )
consumer_compl_map_merged_sla = consumer_compl_map_merged_sla %>% filter(Timely.response == 'No') %>% mutate(missed_sla = round((timely_response_count / cnt_complaints)*100, 4))
##top25_institution_data_merged_sla_desc = consumer_compl_map_merged_sla %>% group_by(Product,Tier,Year,Company) %>%   arrange(Product,Tier,Year,Company,-missed_sla_ratio)

saveRDS(consumer_compl_map_merged_sla,".\\Shinyproject\\data\\consumer_compl_map_merged_sla.rds")

#------------------------------------------------------------------------------------------------------------
# Datasets for graphing Responses tab

consumer_compl_by_prod_year = consumer_compl_prod %>%  group_by(Product,Year) %>%  dplyr::summarise(  cnt_complaints = n())
rr = reshape2::dcast(consumer_compl_by_prod_year,Year ~ Product, value.var = "cnt_complaints" ) 

#To compute Missed SLA % by product and year 
consumer_compl_by_prod_year_timeresp = consumer_compl_prod %>%  group_by(Product,Year,Timely.response) %>%  dplyr::summarise( late_responses = n())
merged_consumer_compl_by_prod_year_timeresp = merge(consumer_compl_by_prod_year,consumer_compl_by_prod_year_timeresp, by = c("Product","Year") )
merged_consumer_compl_by_prod_year_timeresp = merged_consumer_compl_by_prod_year_timeresp %>%  filter(Timely.response == 'No') %>% mutate(missed_sla = round((late_responses / cnt_complaints), 4))
reshaped_late_response = reshape2::dcast(merged_consumer_compl_by_prod_year_timeresp,Year ~ Product, value.var = "missed_sla" ) 
saveRDS(reshaped_late_response,".\\Shinyproject\\data\\reshaped_late_response.rds")

#To compute Disputed Responses % by product and year 
consumer_compl_by_prod_year_consdisp = consumer_compl_prod %>%  group_by(Product,Year,Consumer.disputed) %>%  dplyr::summarise( disputed_responses = n())
merged_consumer_compl_by_prod_year_consdisp = merge(consumer_compl_by_prod_year,consumer_compl_by_prod_year_consdisp, by = c("Product","Year") )
merged_consumer_compl_by_prod_year_consdisp = merged_consumer_compl_by_prod_year_consdisp %>%  dplyr::mutate(bool = Consumer.disputed =='Yes') %>%  filter(bool == TRUE)  %>%   dplyr::mutate(dispute_ratio = round((disputed_responses / cnt_complaints), digits = 4 ))
reshaped_disputed_response = reshape2::dcast(merged_consumer_compl_by_prod_year_consdisp,Year ~ Product, value.var = "dispute_ratio" ) 
saveRDS(reshaped_disputed_response,".\\Shinyproject\\data\\reshaped_disputed_response.rds")


#To compute % of cases closed with a Monetary Relief by product and year 
consumer_compl_by_prod_year_monetary = consumer_compl_prod %>%  group_by(Product,Year,Company.response.to.consumer) %>%  dplyr::summarise( monetary_relief_count = n())
merged_consumer_compl_by_prod_year_monetary = merge(consumer_compl_by_prod_year,consumer_compl_by_prod_year_monetary, by = c("Product","Year") )
merged_consumer_compl_by_prod_year_monetary = merged_consumer_compl_by_prod_year_monetary %>%  dplyr::mutate(bool = Company.response.to.consumer =='Closed with monetary relief') %>%  filter(bool == TRUE)  %>%   dplyr::mutate(monetary_relief = round((monetary_relief_count / cnt_complaints), digits = 4 ))
reshaped_monetary_relief = reshape2::dcast(merged_consumer_compl_by_prod_year_monetary,Year ~ Product, value.var = "monetary_relief" ) 
saveRDS(reshaped_monetary_relief,".\\Shinyproject\\data\\reshaped_monetary_relief.rds")



