######################################################
#################### by Shu Liu ######################
############ shutel at hotmail dot com ###############
################### 08/14/2016 #######################
### Webscraping project @ NYC Data Science Academy ###
######################################################

companies <- read.csv('companies1.csv', stringsAsFactors = FALSE) # load the data
companies <- companies[, -1] # 1353 rows

# tidy up the format
companies$accel_amt_exited <- gsub("\\[u'|\\']", "", companies$accel_amt_exited)
companies$accel_amt_funded <- gsub("\\[u'|\\']", "", companies$accel_amt_funded)
companies$accel_num_exited <- gsub("\\[u'|\\']", "", companies$accel_num_exited)
companies$accel_num_funded <- gsub("\\[u'|\\']", "", companies$accel_num_funded)
companies$accel_estb_yr <- gsub("\\[u'|\\']", "", companies$accel_estb_yr)
companies$accel_estb_yr <- gsub("\\[|\\]", "", companies$accel_estb_yr)
companies$name <- gsub("\n", "", companies$name)

save(companies, file = './companies_mdf1.RData')


