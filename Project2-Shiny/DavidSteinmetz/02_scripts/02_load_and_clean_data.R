# NYCDSA Shiny project
# David Richard Steinmetz
# davidsteinmetz@gmail.com
# Last updated on: 2016-07-28

# Set working directory to Github dir
if (getwd()=='D:/Projects/DataScienceBootcamp/22_Project2_Shiny/') {
    setwd('02_Selfgen/01_App/')
}

# Source ------------------------------------------------------------------

source('02_scripts/01_load_libraries.R')

# Create csv files for optimized load
if (!file.exists('01_data/accident.csv')) {
    acc <- as.data.table(read.dbf('01_data/accident.dbf'))
    write.csv(acc, '01_data/accident.csv')
}
if (!file.exists('01_data/acc_state.csv')) {
    acc_state <- as.data.table(read.xlsx2(
        '01_data/DEFS2009.xls',
        sheetName = 'STATE',
        colClasses = c('integer', 'character')))
    write.csv(acc_state, '01_data/acc_state.csv')
}
if (!file.exists('01_data/accident_merged.csv')) {
    acc <- fread('01_data/accident.csv')
    acc_state <- fread('01_data/acc_state.csv')
    acc <- merge(acc,acc_state,by.x='STATE',by.y='ID') # insert state names for map
    write.csv(acc, '01_data/accident_merged.csv')
}

# Get population data from US Census
if (!file.exists('01_data/NST-EST2014-01.csv')) {
    pop <- fread('01_data/NST-EST2014-01.csv', 
                 skip = 3, 
                 header = TRUE, 
                 nrows = 56)
    pop <- pop[,.(V1,`2014`)]
    pop$V1 <- gsub('[.]', '', pop$V1)
    pop$`2014` <- as.integer(gsub('[,]','', pop$`2014`))
    names(pop) <- c('STATE', 'POP_2014')
    write.csv(pop, '01_data/pop2014.csv')
}


# Get transportation data from BLS
if (!file.exists('01_data/oes_research_2015_sec_48-49.xlsx')) {
    jobs <- fread('01_data/oes_research_2015_sec_48-49.csv',
                  na.strings = c('**','*','#'), 
                  select = c('area','area_title','tot_emp'))
    jobs$tot_emp <- as.integer(gsub('[,]', '', jobs$tot_emp))
    write.csv(jobs, '01_data/trans_jobs_2014.csv')
}


# Optimized load - accidents data and var names
acc <- fread('01_data/accident_merged.csv')
acc[,V1:=NULL] # erase row num column
pop <- fread('01_data/pop2014.csv', select = c('STATE','POP_2014'))
jobs <- fread('01_data/trans_jobs_2014.csv', 
              select = c('area','area_title','tot_emp'))


# Cleanup environment -----------------------------------------------------

#rm(acc)





