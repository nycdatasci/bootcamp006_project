setwd("/Users/williambartlett/PycharmProjects/Web_scraping_project/Excel_files")
library(readxl)
library(dplyr)
library(ggplot2)
library(stats)
library(stringr)
library(tidyr)

activity = read_excel('activity.xlsx')
View(activity)
colnames(activity) = c("Company", "Activity")


activity$Activity = gsub(", u", "SEP", activity$Activity)
activity$Activity = gsub("SEP", " SEP ", activity$Activity)
activity$Activity = gsub("SEP", " ~~~ ", activity$Activity)
activity$Activity = gsub("'", "", activity$Activity)
activity$Activity = gsub("{u", "", activity$Activity, fixed = T)
activity$Activity = gsub("{u", "", activity$Activity, fixed = T)
activity$Activity = gsub("{", "", activity$Activity, fixed = T)
activity$Activity = gsub("}", "", activity$Activity, fixed = T)
activity$Activity = gsub("u\\n", "", activity$Activity, fixed = T)
activity$Activity = gsub("\\n", "", activity$Activity, fixed = T)

activity$activity1 = activity$Activity

activity = separate(data = activity, 
                    col = activity1, 
                    into = as.character(c(1:124)), 
                    sep = " ~~~ ")

max(str_count(activity$Activity, ' ~~~ ')) + 1
