library(data.table)

# Accident data
acc <- fread('01_data/accident_merged.csv') # import
acc[,V1:=NULL] # erase row num column
acc <- acc[,.(STATE,MONTH,DAY,DAY_WEEK,HOUR,LATITUDE,LONGITUD,DEF)] # subset columns
setkey(acc,DEF) # set state names (char) as key to speed sorting and indexing

# Population data
pop <- fread('01_data/pop2014.csv', select = c('STATE','POP_2014'))
pop <- pop[,.(STATE,POP_2014)]
setkey(pop,STATE)

# Transportation data
jobs <- fread('01_data/trans_jobs_2014.csv', 
              select = c('area','area_title','tot_emp'))
setkey(jobs, area_title)

# create variable with colnames as choice
choice <- colnames(acc)

