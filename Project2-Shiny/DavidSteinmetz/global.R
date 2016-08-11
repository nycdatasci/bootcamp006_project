library(data.table)

# Accident data
acc <- fread('01_data/accident_merged.csv',
             select = c('STATE','MONTH','DAY','DAY_WEEK','HOUR','LATITUDE',
                        'LONGITUD','DEF')) # import subset of columns
acc_txt <- fread('01_data/accident_text.csv',
                 select = c('State','Month','Day','Day_Week','Hour',
                            'Latitude','Longitude'))
setkey(acc,DEF) # set state names (char) as key to speed sorting and indexing
setkey(acc_txt,State)

# Population data
pop <- fread('01_data/pop2014.csv', select = c('STATE','POP_2014'))
setkey(pop,STATE)

# Transportation data
jobs <- fread('01_data/trans_jobs_2014.csv', 
              select = c('area','area_title','tot_emp','naics_title'))
setkey(jobs, area_title)