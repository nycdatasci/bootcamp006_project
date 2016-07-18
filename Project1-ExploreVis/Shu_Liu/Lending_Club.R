library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)

# Preparartion of valid data
loandf_07_11 <- read.csv('data/LoanStats07-11.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_12_13 <- read.csv('./data/LoanStats12-13.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_14 <- read.csv('./data/LoanStats14.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_15 <- read.csv('./data/LoanStats15.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_16Q1 <- read.csv('./data/LoanStats16Q1.csv', stringsAsFactors = FALSE, header = TRUE)
loandf <- rbind(loandf_07_11, loandf_12_13, loandf_14, loandf_15, loandf_16Q1)
save(loandf, file = 'loandf.RData')

# Download data
load("loandf.RData")

# Preprocessing -- standardize date format
loandf$issue_d <- as.Date(gsub("^", "01-", loandf$issue_d), format="%d-%b-%y")

# Loan Amount and volume changs from 2007 to 2015
amt_df <- loandf %>%
  select(issue_d, loan_amnt) %>%
  group_by(issue_d) %>%
  summarise(amount = sum(loan_amnt), volume = n(), avgAmt = amount/volume) 
  
## changes of amount
g_amt <- ggplot(amt_df, aes(x = issue_d)
g_amt + geom_line(aes(y = amount), color = 'red') + labs(title = 'Loan amount by month', x = 'Date Issued', y = 'Amount($)')

## changes of volume
g_vol <- ggplot(amt_df, aes(x = issue_d))
g_vol + geom_line(aes(y = volume), color = 'red') + labs(title = 'Loan volume by month', x = 'Date Issued', y = 'Volume')

## changes of average amount per loan
g_avgAmt <- ggplot(amt_df, aes(x = issue_d, y = avgAmt))
g_avgAmt + geom_point(color = 'cadetblue4', size = 0.5) + geom_smooth(color = 'red', linetype = 'dashed', size = 0.7, se = FALSE) + labs(title = 'Average loan amount by month', x = 'Date Issued', y = 'avgAmount')

#### Problems here!

#loan issued locations by volume
loc_df <- select(loandf, addr_state)
loc_df <- loc_df %>%
  group_by(addr_state) %>%
  dplyr::summarise(value = n())

addr_state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
               "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
               "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
               "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

region =  c("alabama",        "alaska",         "arizona",        "arkansas",       "california",     "Colorado",      
                "connecticut",    "Delaware",       "Florida",        "Georgia",        "Hawaii",         "Idaho",
                "Illinois",       "Indiana",        "Iowa",           "Kansas",         "Kentucky",       "Louisiana",     
                "Maine",          "Maryland",       "Massachusetts",  "Michigan",       "Minnesota",      "Mississippi",   
                "Missouri",       "Montana",        "Nebraska",       "Nevada",         "New Hampshire",  "New Jersey",    
                "New Mexico",     "New York",       "North Carolina", "North Dakota",   "Ohio",           "Oklahoma",      
                "Oregon",         "Pennsylvania",   "Rhode Island",   "South Carolina", "South Dakota",   "Tennessee",     
                "Texas",          "Utah",           "Vermont",        "Virginia",       "Washington",     "West Virginia", 
                "Wisconsin",      "Wyoming")
nameTrans <-  data.frame(addr_state, region)
loc_df <- inner_join(loc_df, nameTrans, by = 'addr_state')

state_choropleth(loc_df, title = 'Loan volume by state') 
## how to draw a map
 

# Relation between return rate and default rate and grade and term
## Get numeric value in term columns
deft_df <- loandf %>% 
  select(grade, loan_status, term) %>%
  mutate(term = ifelse(term == ' 36 months', 36, 60))
            
deftGrdTrm_df <- deft_df %>% 
  group_by(grade) %>%
  summarise(defaultRt = sum(loan_status == 'default')/n())
sum(deft_df, loan_status == 'default')

### graph
dft_g <- ggplot(data = deft_df, aes(x = grade, y = defaultRt) ) + geom_point(aes(size = term))

# Grade with factors including Homwowner, Income percent, ...... etc. fico;