library(dplyr)
library(chron) # tidy time format

# Input data
crimedf_11 <- read.csv('data/LAPD_Crime_and_Collision_Raw_Data_for_2011.csv', stringsAsFactors = FALSE, header = TRUE)
crimedf_12_15 <- read.csv('data/Crimes_2012-2015.csv', stringsAsFactors = FALSE, header = TRUE)

# A glimpse of data
ncol(crimedf_11)
ncol(crimedf_12_15)
colnames(crimedf_11)
colnames(crimedf_12_15)

#[1] "Date.Rptd"    "DR.NO"        "DATE.OCC"     "TIME.OCC"     "AREA"
#[6] "AREA.NAME"    "RD"           "Crm.Cd"       "CrmCd.Desc"   "Status"
#[11] "Status.Desc"  "Address"      "Cross.Street" "LOCATION"

# Merge data
crimedf <- rbind(crimedf_11, crimedf_12_15)

# Save data
save(crimedf, file = 'crimedf.RData')





########## Download data ############
load('crimedf.RData')

# Date transform
crimedf$DATE.OCC1 <- as.Date(crimedf$DATE.OCC, format = '%m/%d/%y') 

# Time transform
dig6 <- sprintf('%06d', crimedf$TIME.OCC) # Change 'TIMM.OCC' value to a 6-digit number
dig6 <- gsub('000001$', '000100', dig6) # After data checking, 1 should represent 1:00
time = sub('(\\d{2})(\\d{2})(\\d{2})', '\\2:\\3:00', dig6) # Change 'dig6' to the format '--:--:--'
crimedf$TIME.OCC1 <- chron(times = time) # Finally transformed to 'times' type of the variable 'TIME.OCC'

# Data train
crimedf1 <- crimedf[sample(nrow(crimedf), 10000), ]
save(crimedf1, file = 'crimedf1.RData')

# Daily count (Calender Chart)
# dycount_df <- crimedf %>% group_by(DATE.OCC1) %>%
#                          summarise(cnt = n())
# dyctorder <- dycount_df[order(dycount_df$cnt, decreasing = TRUE), ]

# dyctrm <- dyctorder[c(-1, -2, -1794, -1795),]


