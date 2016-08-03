# convert matrix to dataframe






library(dplyr)
filename = "/Users/dk1306/Downloads/H-1B_Disclosure_Data_FY16.csv"
data = read.csv(filename)

transform = function(x) {
  if(x=="Hour") {
    return(2080)
  }else if(x == "") {
    return(260)
  }else if(x == "Week") {
    return(52)
  }else if(x == "Bi-Weekly") {
    return(26)
  }else if(x == "Month") {
    return(12)
  }else{
    return(1)
  }
}

H1B = data %>% 
      filter(VISA_CLASS == "H-1B") %>%
      select(CASE_STATUS,CASE_SUBMITTED,DECISION_DATE,EMPLOYER_NAME,EMPLOYER_ADDRESS, EMPLOYER_CITY,EMPLOYER_STATE,
             EMPLOYER_POSTAL_CODE, EMPLOYER_COUNTRY,EMPLOYER_PROVINCE, JOB_TITLE,SOC_NAME,TOTAL_WORKERS,PW_UNIT_OF_PAY,
             PREVAILING_WAGE,WILLFUL_VIOLATOR, WORKSITE_CITY,WORKSITE_COUNTY,WORKSITE_STATE,WORKSITE_POSTAL_CODE,
             AGENT_ATTORNEY_NAME,AGENT_ATTORNEY_CITY,AGENT_ATTORNEY_STATE)



H1B = H1B  %>%
      filter(!(EMPLOYER_STATE=="DC" | EMPLOYER_STATE=="GU" | EMPLOYER_STATE=="VI" | EMPLOYER_STATE=="PW" | EMPLOYER_STATE=="PR" | EMPLOYER_STATE=="MP"))  %>%
      filter(EMPLOYER_COUNTRY=="UNITED STATES OF AMERICA") %>%
      filter(!(PREVAILING_WAGE == 0)) %>%
      filter(!(PW_UNIT_OF_PAY == "")) %>%
      filter(!(PW_UNIT_OF_PAY == "Hour" & PREVAILING_WAGE >1000)) %>%
      filter(!(PW_UNIT_OF_PAY == "Week" & PREVAILING_WAGE <200)) %>%
      filter(!(PW_UNIT_OF_PAY == "Bi-Weekly" & PREVAILING_WAGE <200)) %>%
      filter(!(PW_UNIT_OF_PAY == "Month" & PREVAILING_WAGE > 25000)) %>%
      filter(!(PW_UNIT_OF_PAY == "Year" & PREVAILING_WAGE <100))
      


H1B = H1B %>%
      mutate(PW_FREQUENCY_OF_PAY= sapply(PW_UNIT_OF_PAY,transform)) %>%
      mutate(PREVAILING_WAGE= PREVAILING_WAGE*PW_FREQUENCY_OF_PAY)


H1B = H1B %>%
  mutate(PROCESSING_TIME=as.numeric(as.Date(as.character(H1B$DECISION_DATE), format="%m-%d-%y") - as.Date(as.character(H1B$CASE_SUBMITTED), format="%m-%d-%y")))

#H1B = H1B  %>%
  #filter(!(PROCESSING_TIME == 0))

##################################################
d = H1B %>%
    group_by(EMPLOYER_STATE)%>%
    summarise("AVERAGE WAGE"= as.integer(mean(PREVAILING_WAGE)))

temp= H1B %>%
      group_by(EMPLOYER_STATE)%>%
      filter(CASE_STATUS=="CERTIFIED") %>%
      count()
d["CERTIFIED"] = temp$n

temp= H1B %>%
  group_by(EMPLOYER_STATE)%>%
  filter(CASE_STATUS=="CERTIFIED-WITHDRAWN") %>%
  count()
d["CERTIFIED-WITHDRAWN"] = temp$n

temp= H1B %>%
  group_by(EMPLOYER_STATE)%>%
  filter(CASE_STATUS=="WITHDRAWN") %>%
  count()
d["WITHDRAWN"] = 0

for (i in 1:length(temp$EMPLOYER_STATE))
  d$WITHDRAWN[d$EMPLOYER_STATE == as.character(temp$EMPLOYER_STATE[i])] = temp$n[i]



temp= H1B %>%
  group_by(EMPLOYER_STATE)%>%
  filter(CASE_STATUS=="DENIED") %>%
  count()
d["DENIED"] = temp$n


d = d %>% 
    mutate("Total Applications"= d$"CERTIFIED"+d$"CERTIFIED-WITHDRAWN"+ d$"WITHDRAWN"+d$"DENIED")

d = d %>%
  mutate("Approval Percentage"= as.integer((d$"CERTIFIED"+d$"CERTIFIED-WITHDRAWN")/d$"Total Applications"*100))

colnames(d)[1] = "state.name"

###################################################


state_stat = d
state_stat = state_stat[c(1,7,3,4,5,6,8,2)]
state_stat$state.name = as.character(state_stat$state.name)
for (i in 1:50){
  state_stat$state.name[state_stat$state.name == state.abb[i]] = state.name[i]
} 
choice1 <- colnames(state_stat)[-1]


H1B$EMPLOYER_STATE = as.character(H1B$EMPLOYER_STATE)
for (i in 1:50){
  H1B$EMPLOYER_STATE[H1B$EMPLOYER_STATE == state.abb[i]] = state.name[i]
} 



############################################################################################


certified_occ = H1B %>%
    filter(CASE_STATUS=="CERTIFIED-WITHDRAWN" | CASE_STATUS=="CERTIFIED")%>%
    group_by(SOC_NAME) %>%
    count()%>%
    arrange(desc(n))

colnames(certified_occ)[1] = "OCCUPATIONS"
colnames(certified_occ)[2] = "Number of Approved Applications"

certified_state = H1B %>%
  filter(CASE_STATUS=="CERTIFIED-WITHDRAWN" | CASE_STATUS=="CERTIFIED")%>%
  group_by(EMPLOYER_STATE) %>%
  count()%>%
  arrange(desc(n))

colnames(certified_state)[1] = "STATES"
colnames(certified_state)[2] = "Number of Approved Applications"


certified_employers = H1B %>%
  filter(CASE_STATUS=="CERTIFIED-WITHDRAWN" | CASE_STATUS=="CERTIFIED")%>%
  group_by(EMPLOYER_NAME) %>%
  count()%>%
  arrange(desc(n))

colnames(certified_employers)[1] = "EMPLOYERS"
colnames(certified_employers)[2] = "Number of Approved Applications"

certified_job_title = H1B %>%
  filter(CASE_STATUS=="CERTIFIED-WITHDRAWN" | CASE_STATUS=="CERTIFIED")%>%
  group_by(JOB_TITLE) %>%
  count()%>%
  arrange(desc(n))

colnames(certified_job_title)[1] = "JOB TITLES"
colnames(certified_job_title)[2] = "Number of Approved Applications"



############################################################################################

choice2 = c("STATES","OCCUPATIONS","EMPLOYERS","JOB TITLES")


avg_salary_occ = H1B %>%
  group_by(SOC_NAME) %>%
  summarise(av = mean(PREVAILING_WAGE))%>%
  arrange(desc(av))

colnames(avg_salary_occ)[1] = "OCCUPATIONS"
colnames(avg_salary_occ)[2] = "Average Salary"

avg_salary_state = H1B %>%
  group_by(EMPLOYER_STATE) %>%
  summarise(av = mean(PREVAILING_WAGE))%>%
  arrange(desc(av))

colnames(avg_salary_state)[1] = "STATES"
colnames(avg_salary_state)[2] = "Average Salary"


avg_salary_employers = H1B %>%
  group_by(EMPLOYER_NAME) %>%
  summarise(av = mean(PREVAILING_WAGE))%>%
  arrange(desc(av))

colnames(avg_salary_employers)[1] = "EMPLOYERS"
colnames(avg_salary_employers)[2] = "Average Salary"

avg_salary_job_title = H1B %>%
  group_by(JOB_TITLE) %>%
  summarise(av = mean(PREVAILING_WAGE))%>%
  arrange(desc(av))

colnames(avg_salary_job_title)[1] = "JOB TITLES"
colnames(avg_salary_job_title)[2] = "Average Salary"

##############################################################################


library(plotly)
library(dygraphs)
require(xts)

ts= H1B %>% 
  group_by(CASE_SUBMITTED) %>%
  count()

colnames(ts)[2] = "Submitted"
ts$CASE_SUBMITTED = as.Date(as.character(ts$CASE_SUBMITTED), format="%m-%d-%y")



ts = ts %>%
  arrange(desc(CASE_SUBMITTED))%>%
  head(300)


data1 <- xts(ts,order.by = ts$CASE_SUBMITTED) 

ts= H1B %>% 
  group_by(DECISION_DATE) %>%
  count()

colnames(ts)[2] = "Decided"
ts$DECISION_DATE = as.Date(as.character(ts$DECISION_DATE), format="%m-%d-%y")


ts = ts %>%
  arrange(desc(DECISION_DATE))%>%
  head(300)

data2 <- xts(ts,order.by = ts$DECISION_DATE) 

ts_data <- cbind(data1,data2)




PT = H1B %>% group_by(PROCESSING_TIME) %>% count()
colnames(PT)[1] = "Processing Time (days)"
colnames(PT)[2] = "Number of Applications"

