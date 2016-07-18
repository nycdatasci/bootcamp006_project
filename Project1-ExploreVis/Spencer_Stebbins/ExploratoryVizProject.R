###################
#load dependencies#
###################

library(dplyr)
library(reshape2)
library(scales)
library(plotly)

###--------------------
#THESIS
###-------------------
#show inrease student loan debt
#show increasse average borrowing 
#show increase percent of student borrowing
#show increase percent of that is federal
#show percentage of US assests made up by student loans
#show decrease student loan rates
#show decresing earnings for graduates
#show increassing tuition rates
#compare outstanding student loans to outstanding mortgages
#compare deliquencies rates respectively
#compare volume of MBS and SLABS
#(determine starting events for the trends)
#correlation between loans and delinquency and ABS, then bewteen mortgage and student over similar time frame
#show percentage of bank portfolio made up of MBS and percent of Gov made up of student loans

##-------------------------------------
##PLOT 1 - Outsanding Student Loan Debt
##-------------------------------------
#total outstanding student loan debt
student_loans_outstanding_df <- read.csv("https://fred.stlouisfed.org/data/SLOAS.csv")
student_loans_outstanding_df <- transmute(student_loans_outstanding_df, Date=DATE, student_loans_outstanding=VALUE)
#convert to trillions
student_loans_outstanding_df$student_loans_outstanding = student_loans_outstanding_df$student_loans_outstanding * 100000
student_loans_outstanding_df$Date = as.Date(student_loans_outstanding_df$Date)
ggplotly(ggplot(student_loans_outstanding_df, aes(x=Date,y=student_loans_outstanding) ) +geom_line( color='red') + ylab("") + ggtitle("Outstanding Student Loan Debt (Dollars)") + scale_x_date(breaks = date_breaks("5 year"), labels = date_format("%Y")))



state_college_data <- read.csv("./data/CollegeInSight_Explore.csv", stringsAsFactors = FALSE)
#parse state names
state_college_data$Name = gsub(' - 4-year or above', '', state_college_data$Name)
#rename Year column to Date and Name to State
state_college_data <- rename(state_college_data, Date=Year, State=Name)
#convert date to year format
state_college_data$Date = substr(state_college_data$Date,0, nchar(state_college_data$Date) -3)

#student loan deliquencies per state
state_student_loan_delinquencies_data <- read.csv("./data/FRBNY-HDC_STLOANDEL.csv", stringsAsFactors = FALSE)
#melt state column to single column 
state_student_loan_delinquencies_data  <- melt(state_student_loan_delinquencies_data, id= 'Date', variable.name='State', value.name="Percent.student.loan.delinquences")
#convert date to year format
state_student_loan_delinquencies_data$Date = as.character(state_student_loan_delinquencies_data$Date)
state_student_loan_delinquencies_data$Date = substr(state_student_loan_delinquencies_data$Date,0, nchar(state_student_loan_delinquencies_data$Date) -6)

#combine delinquencies with state data frame
state_data <- merge(state_college_data,state_student_loan_delinquencies_data,by=c('Date','State'))

#convert all NA in order to summarize on year
NAs <- state_data == "N/A"
state_data[NAs] <- NA
#convert appropriate to numerics
state_data[,3:11] <- as.numeric(unlist(state_data[,3:11]))

state_data <- select(state_data, -State)
#group by on year and summarize means
data <- state_data %>% group_by(Date) %>% summarise_each(funs(mean(., na.rm = TRUE))) 

##-------------------------------------
##PLOT 2 - Average Student Loan Debt
##-------------------------------------
average_debt <- select(data,Date, Average.debt.of.graduates)
average_debt<- melt(average_debt,id = 'Date', value.name='Amount.in.Thousands')
ggplotly(ggplot(average_debt,aes(x=Date,y=Amount.in.Thousands,group=variable,color=variable)) + geom_line()  + ggtitle('Average Student Loan Debt') + theme(legend.position="none"))

##-------------------------------------
##PLOT 3 - Percent of Student with Debt
##-------------------------------------
percent_student_with_debt <- select(data,Date, Percent.of.graduates.with.debt)
percent_student_with_debt <- melt(percent_student_with_debt ,id = 'Date', value.name='Percent')
ggplotly(ggplot(percent_student_with_debt , aes(x=Date,y=Percent,group=variable,color=variable)) + geom_line() + ggtitle('Percent of Students with Debt') +  theme(legend.position="none"))

##-------------------------------------
##PLOT 4 -  Federal Loan Borrowing as Percent of Total Debt
##-------------------------------------
percent_federal_debt <- select(data,Date, Federal.debt.of.graduates..as.percent.of.total.debt)
percent_federal_debt <- melt(percent_federal_debt ,id = 'Date', value.name='Percent')
ggplotly(ggplot(percent_federal_debt , aes(x=Date,y=Percent,group=variable,color=variable)) + geom_line() + ggtitle('Federal Loan Borrowing as Percent of Total Debt') +  theme(legend.position="none"))


##-------------------------------------
##PLOT 5 - Increasing College Costs
##-------------------------------------
increasing <- select(data,Date,Total.cost.of.attendance..on.campus.)
increasing <- melt(increasing,id = 'Date', value.name='Amount.in.Thousands')
ggplotly(ggplot(increasing, aes(x=Date,y=Amount.in.Thousands,group=variable,color=variable)) + geom_line() + ggtitle('Average College Costs') + theme(legend.position="none"))




#-------------------------------------------------------------
#LATER WORK
#-------------------------------------------------------------

#mortgage debt
mortgage_debt_outstanding_df <- read.csv("https://fred.stlouisfed.org/data/MDOAH.csv", stringsAsFactors = FALSE)
mortgage_debt_outstanding_df <- transmute(mortgage_debt_outstanding_df, Date=DATE, mortgage_debt_outstanding=as.numeric(VALUE))
#convert to trillions
mortgage_debt_outstanding_df$mortgage_debt_outstanding = mortgage_debt_outstanding_df$mortgage_debt_outstanding * 100

#join debt outstanding data frames
debt_df <- join_all(list(mortgage_debt_outstanding_df,student_loans_outstanding_data), by='Date', type = "full")
#convert date
debt_df$Date = as.Date(debt_df$Date)
#melt data on date
debt_data <- melt(debt_df,id = 'Date', measure.vars = names(select(debt_df, -Date)))
#plot graph
ggplotly(ggplot(debt_data, aes(x=Date,y=value,group=variable,color=variable)) + geom_line() + facet_wrap(~variable, scales = "free"))
  
#-------------------------------------------------------------
#compare deliquencies rates respectively
#-------------------------------------------------------------

#mortgage delinquencies
mortgage_delinquencies_df <- read.csv("https://fred.stlouisfed.org/data/DRSFRMACBS.csv")
mortgage_delinquencies_df <- transmute(mortgage_delinquencies_df, Date=DATE, mortgage_delinquencies=VALUE)

#join delinquency data frames
delinquencies_df <- join_all(list(mortgage_delinquencies_df, student_loan_delinquencies_data), by='Date', type = "full")
#convert date
delinquencies_df$Date = as.Date(delinquencies_df$Date)
#melt data on date
delinquencies_data <- melt(delinquencies_df,id = 'Date', measure.vars = names(select(delinquencies_df, -Date)))
delinquencies_data <- na.omit(delinquencies_data)
#plot graph
ggplotly(ggplot(delinquencies_data, aes(x=Date,y=value,group=variable,color=variable) ) + geom_line() + scale_x_date(breaks = date_breaks("5 year"), labels = date_format("%Y")))

#-------------------------------------------------------------
# compare volume of MBS and SLABS
#-------------------------------------------------------------


#mortgage rates
# mortgage_rates_df <- read.csv("~/Documents/NYCDSA/Exploratory Visualization Project/FMAC-MORTG.csv")
# mortgage_rates_df <- transmute(mortgage_rates_df, date=Date, mortgage_rates=Value)

