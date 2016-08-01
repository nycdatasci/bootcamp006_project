library(sas7bdat)
library(dplyr)
library(reshape2)

setwd("/Users/cholmes/Desktop/DD/")

statepop = read.csv("/Users/cholmes/Desktop/StatePopData.csv")

#Use this to read in the SAS files from FARS, otherwise us the csv data from github.
#years = c(1999, 2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)

#df = data.frame()
#for (year in years) {
#  df1 = read.sas7bdat(paste('FARS ', year, '/acc_aux.sas7bdat', sep=''))
#  print(year)
  
#  if (year > 2012) {
#    drops = c('A_PEDAL_F', 'A_PED_F')
#    df1 = df1[, !(names(df1) %in% drops)]
#  }
#  df = rbind(df, df1)
#}


df = read.csv('/Users/cholmes/Desktop/df.csv')
clean = select(df, YEAR, STATE, FATALS, A_DIST, A_SPCRA, A_POSBAC)
clean$YEAR = as.character(clean$YEAR)

#All Fatalities by year
total_fatal = summarise(group_by(clean, YEAR), FATALS = sum(FATALS))

#Drunk Driving
dd = mutate(clean, A_POSBAC=replace(A_POSBAC, A_POSBAC>1, 0))
dd = mutate(dd, drunk_fatals = FATALS*A_POSBAC)
dd_fatal = summarise(group_by(dd, YEAR), FATALS = sum(drunk_fatals))

#Distracted Driving
distract = mutate(clean, A_DIST=replace(A_DIST, A_DIST>1, 0))
distract = mutate(distract, distract_fatals = FATALS*A_DIST)
distract_fatal = summarise(group_by(distract, YEAR), FATALS = sum(distract_fatals))


#Speeding
speeding = mutate(clean, A_SPCRA = replace(A_SPCRA, A_SPCRA > 1, 0))
speeding = mutate(speeding, speeding_fatals = A_SPCRA*FATALS)
speeding_fatal = summarise(group_by(speeding, YEAR), FATALS = sum(speeding_fatals))




##Start of State Prep
state_data = merge(x=clean, y=statepop, by.x=c('YEAR', 'STATE'), by.y=c('Year', 'State'))
state_data = mutate(state_data, A_SPCRA = replace(A_SPCRA, A_SPCRA > 1, 0), A_DIST=replace(A_DIST, A_DIST>1, 0), A_POSBAC=replace(A_POSBAC, A_POSBAC>1, 0))
state_data = mutate(state_data, drunk_fatals = FATALS*A_POSBAC, distract_fatals = FATALS*A_DIST,speeding_fatals = A_SPCRA*FATALS)

grouped_state = summarise(group_by(state_data, YEAR, STATE, Code), total_fatals = sum(FATALS), drunk_fatals = sum(drunk_fatals), distract_fatals = sum(distract_fatals), speeding_fatals = sum(speeding_fatals), Population = mean(Population))
grouped_state = mutate(grouped_state, total_rate = total_fatals*100000/Population, drunk_rate = drunk_fatals*100000/Population, distract_rate = distract_fatals*100000/Population, speeding_rate = speeding_fatals*100000/Population)
mapdat = grouped_state

mapdat = mapdat[mapdat$YEAR >= 2001 & mapdat$YEAR <= 2007,]
mapdat = summarise(group_by(mapdat, Code),  rate = mean(total_rate))

