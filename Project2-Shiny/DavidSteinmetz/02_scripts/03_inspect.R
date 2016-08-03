# NYCDSA Shiny project
# David Richard Steinmetz
# davidsteinmetz@gmail.com
# Last updated on: 2016-07-28

# Set working directory to Github dir
if (getwd()=='D:/Projects/DataScienceBootcamp/22_Project2_Shiny') {
    setwd('02_Selfgen/01_App/')
}

# Source ------------------------------------------------------------------

source('02_scripts/01_load_libraries.R')
source('02_scripts/02_load_and_clean_data.R')

# Inspect imported data ---------------------------------------------------

# Take a look
names(acc)
str(acc)
summary(acc)
NAs <- sum(is.na(acc)) # count NA's in entire dataset
print(paste('Number of NA\'s:',NAs))
print(paste('Percent NA:',round(NAs/(dim(acc)[1]*dim(acc)[2])*100,2)))


# Investigate variables ---------------------------------------------------

# Set parameters
var <- 'MONTH' # variable (column) to investigate; can be int or char
step <- 1 # Used to find empty categories represented by integers

# Preprocess
if (is.integer(var) | is.numeric(var)) { # gather and format column names
    col <- names(acc)[var]
    col_formatted <- capitalize(tolower(col))
} else if (is.character(var)) {
    col <- var
    col_formatted <- capitalize(tolower(col))
    
}
acc_var <- acc[[var]] # create integer vector (var can be int or char)
freq_tab <- acc[,.N,by=col] # Create frequency table of column


# Take a look
str(acc_var) # structure of variable
summary(acc_var) # min, max, median, mean
quantile(acc_var) # summary without mean, easier to view
acc[,col,with=FALSE] # head and tail (data.table's default print)
head(freq_tab,5) # head and tail of frequency table
tail(freq_tab,5)
head(freq_tab[order(N)],5) # head and tail of sorted frequency table
tail(freq_tab[order(N)],5)

# Missing values analysis
# For categorical variables represented by numbers, find non-seq. categories
span <- seq(min(acc_var),max(acc_var),step)
in_data <- span %in% unique(acc_var)
zero_el <- span[!in_data]
zero_el
# NA's
NAs <- sum(is.na(acc_var))
print(paste('Number of NA\'s for ', col, ': ', NAs, sep=''))
print(paste('Percent NA\'s for ',col,': ',round(NAs/dim(acc)[1]*100,2),sep=''))


# Cleanup environment -----------------------------------------------------

rm(var, step, col_formatted, acc_var, span, in_data, zero_el, NAs)


