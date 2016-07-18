# -----------------------------------------------------------------------------
# load minimal needed librarues for plotting
library(ggplot2)
library(dplyr)

# -----------------------------------------------------------------------------
# Mortality rate by State

# Read in the State Summary Totals data set - 167 fields in all
ccmdata <- read.csv('./ccm_statesummt.csv')
dim(ccmdata)
names(ccmdata)

# get the US cancer mortality rates for each state (try filter on NY city first)
ccmrate <- ccmdata %>% select(ABBREV,REGION,ends_with("_RATE"))

# start the bar plot
plot.new()
g <- ggplot(data=ccmrate,aes(x=reorder(ABBREV,-ACCT_RATE),y=ACCT_RATE,fill=ACCT_RATE)) + 
  xlab("State") + ylab("Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")
g

# -----------------------------------------------------------------------------
# Do analysis based on State/ Gender/ Mortality Rate
# need to create a new data set based on above schema

x <- 1
ccmrate2 <- NULL
for (x in 1:50) {
  # fetch the row data from ccmdata
  df1 <- ccmdata[x,c(1,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89,92,95,98,101,104)]
  # form the new data frame to convert the fields
  # female record
  newrow <- data.frame(
     State = df1$ABBREV,
     Gender = "Female",
     Bladder = df1$BLAF_RATE,
     Brain = df1$BRAF_RATE,
     Breast = df1$BREF_RATE,
     Cervix = df1$CERF_RATE,
     Colon = df1$COLF_RATE,
     Esophagus = df1$ESOF_RATE,
     Kidney = df1$KIDF_RATE,
     Leukemia = df1$LEUF_RATE,
     Liver = df1$LGBF_RATE,
     Lung = df1$LUNF_RATE,
     NHL = df1$NHLF_RATE,
     Oral = df1$ORAF_RATE,
     Other = df1$OTHF_RATE,
     Ovary = df1$OVAF_RATE,
     Pancreas = df1$PANF_RATE,
     Prostrate = 0.00,
     Rectum = df1$RECF_RATE,
     Stomach = df1$STOF_RATE,
     Uterus = df1$UTEF_RATE
  )
  # add the new row for females
  ccmrate2 <- bind_rows(ccmrate2,newrow)

  # male record
  newrow <- data.frame(
    State = df1$ABBREV,
    Gender = "Male",
    Bladder = df1$BLAM_RATE,
    Brain = df1$BRAM_RATE,
    Breast = 0.00,
    Cervix = 0.00,
    Colon = df1$COLM_RATE,
    Esophagus = df1$ESOM_RATE,
    Kidney = df1$KIDM_RATE,
    Leukemia = df1$LEUM_RATE,
    Liver = df1$LGBM_RATE,
    Lung = df1$LUNM_RATE,
    NHL = df1$NHLM_RATE,
    Oral = df1$ORAM_RATE,
    Other = df1$OTHM_RATE,
    Ovary = 0.00,
    Pancreas = df1$PANM_RATE,
    Prostrate = df1$PROM_RATE,
    Rectum = df1$RECM_RATE,
    Stomach = df1$STOM_RATE,
    Uterus = 0.00
  )
  # add the new row for females
  ccmrate2 <- bind_rows(ccmrate2,newrow)
  
}

# start plotting the bar charts for each cancer type
ggplot(data=ccmrate2,aes(x=reorder(State,-Bladder),y=Bladder,fill=Gender)) + 
  xlab("State") + ylab("Bladdder Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Brain),y=Brain,fill=Gender)) + 
  xlab("State") + ylab("Brain Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Breast),y=Breast,fill=Gender)) + 
  xlab("State") + ylab("Breast Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Cervix),y=Cervix,fill=Gender)) + 
  xlab("State") + ylab("Cervix Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Colon),y=Colon,fill=Gender)) + 
  xlab("State") + ylab("Colon Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Esophagus),y=Esophagus,fill=Gender)) + 
  xlab("State") + ylab("Esophagus Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Kidney),y=Kidney,fill=Gender)) + 
  xlab("State") + ylab("Kidney Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Leukemia),y=Leukemia,fill=Gender)) + 
  xlab("State") + ylab("Leukemia Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Liver),y=Liver,fill=Gender)) + 
  xlab("State") + ylab("LiverCancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Lung),y=Lung,fill=Gender)) + 
  xlab("State") + ylab("Lung Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-NHL),y=NHL,fill=Gender)) + 
  xlab("State") + ylab("NHL Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Oral),y=Oral,fill=Gender)) + 
  xlab("State") + ylab("Oral Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Other),y=Other,fill=Gender)) + 
  xlab("State") + ylab("Other Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Ovary),y=Ovary,fill=Gender)) + 
  xlab("State") + ylab("Ovary Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Pancreas),y=Pancreas,fill=Gender)) + 
  xlab("State") + ylab("Pancreas Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Prostrate),y=Prostrate,fill=Gender)) + 
  xlab("State") + ylab("Prostrate Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Rectum),y=Rectum,fill=Gender)) + 
  xlab("State") + ylab("Rectum Cancer Mortality Rate (deaths per 100K people)") +  
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Stomach),y=Stomach,fill=Gender)) + 
  xlab("State") + ylab("Stomach Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")

ggplot(data=ccmrate2,aes(x=reorder(State,-Uterus),y=Uterus,fill=Gender)) + 
  xlab("State") + ylab("Uterus Cancer Mortality Rate (deaths per 100K people)") +
  geom_bar(stat="Identity")
