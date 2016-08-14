# David Richard Steinmetz
# Web scraping project
# NYC Data Science Academy
# Last modified: 2016-08-14



# Functions ---------------------------------------------------------------

# Investigate New Dataset
open_sesame <- function(data){
  print('----------------------Variables---------------------')
  print(names(data))
  print('----------------------Structure---------------------')
  str(data)
  print('------------------------Head------------------------')
  print(head(data))
  print('-----------------------Summary----------------------')
  print(summary(data))
  print('-----------------Standard Deviation-----------------')
  num <- sapply(data, class) %in% c('integer','numeric')
  print(sapply(data[num], sd))
  # ----------------------Missingness------------------------
  print('----------------------Total NAs---------------------')
  total_na <- sum(is.na(data))
  if (total_na > 0){
    print(total_na)
  } else {
    print('No missing values')
  }
  print('-------------------Columns with NAs-----------------')
  if (total_na > 0){
    print(names(heptathlon)[colSums(is.na(head(heptathlon))) > 0])
  } else {
    print('No missing values')
  }
  # ---------------Plot pairwise variables-------------------
  plot(data[,num])
}

# Univariate EDA
uni_eda <- function(vec){
  # Clean
  vec <- vec[!is.na(vec)]
  # SOCS - Shape, Outliers, Center, Spread
  if (class(vec) %in% c('integer', 'numeric')) {
    par(mfcol=c(3,1), mfrow=c(1,3))
    plot(density(vec), main='Sample Distribution') # Shape
    hist(vec, main='Sample Distribution') # Shape
    boxplot(vec, main='Sample Distribution') # Outliers
    par(mfcol=c(1,1), mfrow=c(1,1))
    print(summary(vec)) # Center, Spread
    print(paste('Standard deviation:', round(sd(vec),4))) # Spread
    print(paste('Variance:', round(var(vec),4))) # Spread
  } else if (class(vec) %in% c('factor', 'character')) {
    barplot(table(vec)) # Shape
    print(table(vec)) # Shape
    print(summary(vec)) # Center
  }
}

# Wait for Enter keystroke to continue function
readkey <- function()
{
  cat ("Press [enter] to continue, type 'q' to quit")
  line <- readline()
}

# Uni_eda for all columns in a data table
uni <- function(df){
  if (is.data.frame(df)){
    for (col in 1:length(df)) {
      uni_eda(as.vector(df[[col]]))
      print(paste('Variable name:', names(df)[col]))
      if(readkey() == 'q') break
    }
  } else {
    print('Error: Not a data frame')
  }
}


# EDA ---------------------------------------------------------------------

# Load libraries
library(data.table)

# Load data
setwd('D:/Projects/DataScienceBootcamp/23_Project3_Web_Scraping/02_Selfgen/')
dt <- fread('data.csv')

# Transform data
dt[, gender:=as.factor(gender)]
dt[, nat:=as.factor(nat)]
dt[, year:=as.factor(year)]
df = as.data.frame(dt)

# Investigate new dataset
open_sesame(df)


# Univariate EDA
uni(dt)


# Bivariate EDA
isnum = sapply(df, class) %in% c('integer', 'numeric')
library(corrplot)
cor(df[,isnum])
corrplot(cor(df[,isnum]))
plot(df[1:100,])



# Investigate and Clean Outliers ------------------------------------------

# birth_year
dt[birth_year==0]  # View 
dt[birth_year==0, birth_year:=NA]  # All values were missing in the results
dt[is.na(birth_year)]  # View

# nat
dt[nat=='NON']  # View
dt[nat=='NON', nat:=NA]  # All values were missing in the results
dt[is.na(nat)]

# age
dt[age<0]  # View 
dt <- dt[-which(dt$age<0),]  # One falsely read row
dt[age<0]  # View that it's empty
dt[age>100]  # View
dt[age>100, age:=NA]  # Miscalculations due to missing birth_year
dt[age>100]
uni_eda(dt[['age']])  # View new age distribution

