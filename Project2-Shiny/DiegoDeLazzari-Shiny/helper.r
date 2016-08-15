library(dplyr)
library(tidyr)

# setwd("~/Documents/NYCDSA/Project 2")
save.flag = FALSE
##############################
estimatedArrivals2016 = read.csv('data/original/20160713-AnalysisofEstimatedArrivals.csv',
                                 sep = ";",dec = ",", stringsAsFactors = F) 
# Create the Dataset
estArr2016 = tbl_df(estimatedArrivals2016)[1:9]

# Take care of the thousands separator
estArr2016[,2:ncol(estArr2016)] = sapply(estArr2016[,2:ncol(estArr2016)],
                                    function(v){as.numeric(gsub('\\.','',v))})
# Clean and fix Idents
estArr2016 = estArr2016[,-2] %>%
    mutate(.,Date = as.Date(Date,"%m/%d/%y"),
                 Arrivals.to.Slovenia = ifelse(Arrivals.to.Slovenia=="N/A",0,
                                               as.numeric(Arrivals.to.Slovenia)),
                 Arrivals.to.Austria = ifelse(Arrivals.to.Austria=="N/A",0,
                                               as.numeric(Arrivals.to.Austria)))
estArr2016[is.na(estArr2016)]=0.

# Change col name and check header
colnames(estArr2016)[3]="Arrivals.to.Macedonia"

# check for Nas and summarize
any(is.na(estArr2016))
head(estArr2016)

# Save route towards Balkans
if(save.flag){
saveRDS(estArr2016, file = "data/PathGreeceBalkans2016.rds", ascii = FALSE)
}
##############################

# Dataset 2: Breakdown of refugees Nationality
# 
# Load and clean
estimatedOrigin2016 = read.csv('data/original/Origin.csv',
                                 sep = ";",dec = ",",stringsAsFactors = F,nrows = 59) 

estOrig2016 = tbl_df(estimatedOrigin2016)[1:5]%>% select(.,-4)

# Take care of the thousands separator
estOrig2016[,3:ncol(estOrig2016)] = sapply(estOrig2016[,3:ncol(estOrig2016)],
                                         function(v){as.numeric(gsub('\\.','',v))})
# clean
colnames(estOrig2016)[3] = 'Total.2016'
estOrig2016[is.na(estOrig2016)]=0.
estOrig2016$Origin[estOrig2016$Origin == "C<c3><b4>te d'Ivoire"] = "Cote d'Ivoire"

# Add total
estTotal2016 = data.frame(Country = c('Total'), Origin = c('Total'), Total.2016 =
                            c(sum(estOrig2016$Total.2016)),Total.2015 =
                            c(sum(estOrig2016$Total.2015)))

estOrig2016  = bind_rows(estOrig2016,estTotal2016)

# check for Nas and summarize
any(is.na(estOrig2016))
head(estOrig2016)

# Save
if(save.flag){
saveRDS(estOrig2016, file = "data/dataOrigin2016.rds", ascii = FALSE)
}
###############################

# Dataset 3: About gender distribuiton
estimatedGender2016 = read.csv('data/original/DemographyMonthly.csv',
                               sep = ";",dec = ",",stringsAsFactors = F,nrows = 24)

# Remove Cyprus and Malta: no data available
estGend2016 = tbl_df(estimatedGender2016)[1:5]%>% select(.,-3)%>%filter(.,Country != 'Cyprus' & Country!= 'Malta' )

# Take care of the thousands separator
estGend2016[,3:ncol(estGend2016)] = sapply(estGend2016[,3:ncol(estGend2016)],
                                         function(v){as.numeric(gsub('\\.','',v))})
estGend2016[is.na(estGend2016)]=0.

# check for Nas and summarize
any(is.na(estGend2016))
head(estGend2016)

# Save
if(save.flag){
saveRDS(estGend2016, file = "data/dataGender2016.rds", ascii = FALSE)
}
#####

#########################
# Dataset 4
# 
arrivalSea2016 = read.csv('data/original/MonthlyArrivalsByCountry.csv',
         sep = ";",dec = ",", stringsAsFactors = F) 

estArrMedit2016 = arrivalSea2016[c(3,5),c(-2:-6)] %>% 
  gather(.,key="Date",value="Arrivals",2:32) %>% 
  mutate(.,Date = as.yearmon(Date,"%b.%y"))

# Save route towards Balkans
if(save.flag){
  saveRDS(estArrMedit2016, file = "data/PathSpainItaly2016.rds", ascii = FALSE)
}