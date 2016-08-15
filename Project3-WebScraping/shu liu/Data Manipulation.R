######################################################
#################### by Shu Liu ######################
############ shutel at hotmail dot com ###############
################### 08/14/2016 #######################
### Webscraping project @ NYC Data Science Academy ###
######################################################

library(corrplot)
library(dplyr)

load('./companies_mdf1.RData')
accel_corr <- distinct(companies_cln[, c('accel_amt_exited', 'accel_amt_funded', 
                          'accel_num_exited', 'accel_num_funded')])
M <- cor(accel_corr)
corrplot(M, method = 'circle')

startup_corr <- companies_cln[, c('name', 'funding', 'rounds', 'followers_num', 
                                  'friends_num', 'statuses_num')]
startup_corr <- companies_cln[which('followers_num' %in% '0' & 
                         'friends_num' %in% '0' & 'statuses_num' %in% '0')]
