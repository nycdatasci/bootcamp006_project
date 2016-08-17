library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library(googleVis)
library(sjPlot)  ## load package for likert plot


file_names <- dir("./cardpool/") # get all csv file names

## function to read file and add scraping hour extracted from the file name to dataframe
add_file_name = function(file_name){
  df = read.csv(paste0("./cardpool/",file_name))
  df$opt_hour = strsplit(file_name,'_')[[1]][1]
  return (df)
}

## merge all dataframes together
data_merged <- do.call(rbind,lapply(file_names,add_file_name)) # bind the together

data_merged$time=strsplit(toString(data_merged$time),"\\.")[[1]][1] ## remove the time scale smaller than second

## build a function to remove leading and trailing whitespaces from a string
trim <- function(x) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

data_merged$merchant = trim(data_merged$merchant)

cat_list = read.csv("cardpool.csv") ##  get the dataframe with merchant name and category
cat_list = cat_list[,-c(1,3,5)]   ## remove columns not needed
cat_list$merchant = trim(cat_list$merchant)   ## remove heading and tailing whitespace
cat_list$category = as.character(cat_list$category) ##  define category as character

## correct a category name
cat_list$category = ifelse(cat_list$category=="Flowers &","Flowers & Gifts",cat_list$category)

## add category name to dataframe
df = inner_join(data_merged,cat_list,by="merchant")
df = df[,-1] ## further clean the dataframe

df$opt_hour = as.numeric(df$opt_hour) ## define opt_hour as factor 

## 1. prepare data for the Likert chart
avl = unique(df$merchant)  ## available merchant cards
all = cat_list$merchant    ## all merchant cards that have been listed till 8/13/16

## all[!(all %in% avl)]  ## check merchant cards that are not available on Satruday, 08/13/2016
avail = ifelse((all %in% avl),1,0)   ## create a binary variable repsenting availability of merchant 

cat_list_cnt = cat_list   ## add the avail variable
cat_list_cnt$avail = avail

## generate a data frame counting available and unavailable merchants within each category 
df_avail = cat_list_cnt %>% group_by(category) %>% 
           summarize(count=n(),avail_y=sum(avail),avail_n=n()-sum(avail))
