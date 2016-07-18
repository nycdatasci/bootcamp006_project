# -----------------------------------------------------------
# Created by: Bernard Ong
# Created on: July 14, 2016
# Created for: Visualization Project
# Purpose for: Cleaning and normalizing the data source
# Purpose for: Getting standard stats
# Purpose for: generate the maps
# -----------------------------------------------------------

# load needed libraries
library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)

# load helper code function library
source("./helper.R")

# load cancer mortality data set
# ccm2.csv is result of data cleansing using Excel
ccdf <- read.csv('./ccm2.csv')
dim(ccdf)
colnames(ccdf)[1] <- c("ABBREV")
str(ccdf)

# sum up all the states into one line (=50 rows instead) rather than 5905 rows
# this will still have separate M and F stats
StateSumm <- 
  ccdf %>% 
  group_by(ABBREV) %>% 
  summarise(
    ACCF_RATE = round(mean(ACCF_RATE),digits=2),
    ACCF_CNT = sum(ACCF_CNT),
    ACCF_ECNT = sum(ACCF_ECNT),
    ACCM_RATE = round(mean(ACCM_RATE),digits=2),
    ACCM_CNT = sum(ACCM_CNT),
    ACCM_ECNT = sum(ACCM_ECNT),
    BLAF_RATE = round(mean(BLAF_RATE),digits=2),
    BLAF_CNT = sum(BLAF_CNT),
    BLAF_ECNT = sum(BLAF_ECNT),
    BLAM_RATE = round(mean(BLAM_RATE),digits=2),
    BLAM_CNT = sum(BLAM_CNT),
    BLAM_ECNT = sum(BLAM_ECNT),
    BRAF_RATE = round(mean(BRAF_RATE),digits=2),
    BRAF_CNT = sum(BRAF_CNT),
    BRAF_ECNT = sum(BRAF_ECNT),
    BRAM_RATE = round(mean(BRAM_RATE),digits=2),
    BRAM_CNT = sum(BRAM_CNT),
    BRAM_ECNT = sum(BRAM_ECNT),
    BREF_RATE = round(mean(BREF_RATE),digits=2),
    BREF_CNT = sum(BREF_CNT),
    BREF_ECNT = sum(BREF_ECNT),
    CERF_RATE = round(mean(CERF_RATE),digits=2),
    CERF_CNT = sum(CERF_CNT),
    CERF_ECNT = sum(CERF_ECNT),
    COLF_RATE = round(mean(COLF_RATE),digits=2),
    COLF_CNT = sum(COLF_CNT),
    COLF_ECNT = sum(COLF_ECNT),
    COLM_RATE = round(mean(COLM_RATE),digits=2),
    COLM_CNT = sum(COLM_CNT),
    COLM_ECNT = sum(COLM_ECNT),
    ESOF_RATE = round(mean(ESOF_RATE),digits=2),
    ESOF_CNT = sum(ESOF_CNT),
    ESOF_ECNT = sum(ESOF_ECNT),
    ESOM_RATE = round(mean(ESOM_RATE),digits=2),
    ESOM_CNT = sum(ESOM_CNT),
    ESOM_ECNT = sum(ESOM_ECNT),
    KIDF_RATE = round(mean(KIDF_RATE),digits=2),
    KIDF_CNT = sum(KIDF_CNT),
    KIDF_ECNT = sum(KIDF_ECNT),
    KIDM_RATE = round(mean(KIDM_RATE),digits=2),
    KIDM_CNT = sum(KIDM_CNT),
    KIDM_ECNT = sum(KIDM_ECNT),
    LEUF_RATE = round(mean(LEUF_RATE),digits=2),
    LEUF_CNT = sum(LEUF_CNT),
    LEUF_ECNT = sum(LEUF_ECNT),
    LEUM_RATE = round(mean(LEUM_RATE),digits=2),
    LEUM_CNT = sum(LEUM_CNT),
    LEUM_ECNT = sum(LEUM_ECNT),
    LGBF_RATE = round(mean(LGBF_RATE),digits=2),
    LGBF_CNT = sum(LGBF_CNT),
    LGBF_ECNT = sum(LGBF_ECNT),
    LGBM_RATE = round(mean(LGBM_RATE),digits=2),
    LGBM_CNT = sum(LGBM_CNT),
    LGBM_ECNT = sum(LGBM_ECNT),
    LUNF_RATE = round(mean(LUNF_RATE),digits=2),
    LUNF_CNT = sum(LUNF_CNT),
    LUNF_ECNT = sum(LUNF_ECNT),
    LUNM_RATE = round(mean(LUNM_RATE),digits=2),
    LUNM_CNT = sum(LUNM_CNT),
    LUNM_ECNT = sum(LUNM_ECNT),
    NHLF_RATE = round(mean(NHLF_RATE),digits=2),
    NHLF_CNT = sum(NHLF_CNT),
    NHLF_ECNT = sum(NHLF_ECNT),
    NHLM_RATE = round(mean(NHLM_RATE),digits=2),
    NHLM_CNT = sum(NHLM_CNT),
    NHLM_ECNT = sum(NHLM_ECNT),
    ORAF_RATE = round(mean(ORAF_RATE),digits=2),
    ORAF_CNT = sum(ORAF_CNT),
    ORAF_ECNT = sum(ORAF_ECNT),
    ORAM_RATE = round(mean(ORAM_RATE),digits=2),
    ORAM_CNT = sum(ORAM_CNT),
    ORAM_ECNT = sum(ORAM_ECNT),
    OTHF_RATE = round(mean(OTHF_RATE),digits=2),
    OTHF_CNT = sum(OTHF_CNT),
    OTHF_ECNT = sum(OTHF_ECNT),
    OTHM_RATE = round(mean(OTHM_RATE),digits=2),
    OTHM_CNT = sum(OTHM_CNT),
    OTHM_ECNT = sum(OTHM_ECNT),
    OVAF_RATE = round(mean(OVAF_RATE),digits=2),
    OVAF_CNT = sum(OVAF_CNT),
    OVAF_ECNT = sum(OVAF_ECNT),
    PANF_RATE = round(mean(PANF_RATE),digits=2),
    PANF_CNT = sum(PANF_CNT),
    PANF_ECNT = sum(PANF_ECNT),
    PANM_RATE = round(mean(PANM_RATE),digits=2),
    PANM_CNT = sum(PANM_CNT),
    PANM_ECNT = sum(PANM_ECNT),
    PROM_RATE = round(mean(PROM_RATE),digits=2),
    PROM_CNT = sum(PROM_CNT),
    PROM_ECNT = sum(PROM_ECNT),
    RECF_RATE = round(mean(RECF_RATE),digits=2),
    RECF_CNT = sum(RECF_CNT),
    RECF_ECNT = sum(RECF_ECNT),
    RECM_RATE = round(mean(RECM_RATE),digits=2),
    RECM_CNT = sum(RECM_CNT),
    RECM_ECNT = sum(RECM_ECNT),
    STOF_RATE = round(mean(STOF_RATE),digits=2),
    STOF_CNT = sum(STOF_CNT),
    STOF_ECNT = sum(STOF_ECNT),
    STOM_RATE = round(mean(STOM_RATE),digits=2),
    STOM_CNT = sum(STOM_CNT),
    STOM_ECNT = sum(STOM_ECNT),
    UTEF_RATE = round(mean(UTEF_RATE),digits=2),
    UTEF_CNT = sum(UTEF_CNT),
    UTEF_ECNT = sum(UTEF_ECNT)  
  )

# delete 1st row since it's blank and NA's
StateSumm <- StateSumm[-c(1),]

# export the state totals if needed for testing only, with M and F stats, in case we need this in future
# but no state M/f totals yet, next section will calculate
# write.csv(StateSumm,'./ccm_statesumm.csv',row.names=FALSE,quote=FALSE)

# ----------------------------------------------------------------------------------
# sum up all the males and females into one state Total (*T)
StateSummT <- 
  StateSumm %>%
  mutate(
    ABBREV = ABBREV,
    ACCT_RATE = (ACCF_RATE+ACCM_RATE)/2,
    ACCT_CNT = ACCF_CNT + ACCM_CNT,
    ACCT_ECNT = ACCF_ECNT + ACCM_ECNT,
    BLAT_RATE = (BLAF_RATE+BLAM_RATE)/2,
    BLAT_CNT = BLAF_CNT + BLAM_CNT,
    BLAT_ECNT = BLAF_ECNT + BLAM_ECNT,
    BRAT_RATE = (BRAF_RATE+BRAM_RATE)/2,
    BRAT_CNT = BRAF_CNT + BRAM_CNT,
    BRAT_ECNT = BRAF_ECNT + BRAM_ECNT,
    BRET_RATE = BREF_RATE,
    BRET_CNT = BREF_CNT,
    BRET_ECNT = BREF_ECNT,
    CERT_RATE = CERF_RATE,
    CERT_CNT = CERF_CNT,
    CERT_ECNT = CERF_ECNT,
    COLT_RATE = (COLF_RATE+COLM_RATE)/2,
    COLT_CNT = COLF_CNT + COLM_CNT,
    COLT_ECNT = COLF_ECNT + COLM_ECNT,
    ESOT_RATE = (ESOF_RATE+ESOM_RATE)/2,
    ESOT_CNT= ESOF_CNT + ESOM_CNT,
    ESOT_ECNT = ESOF_ECNT + ESOM_ECNT,
    KIDT_RATE = (KIDF_RATE+KIDM_RATE)/2,
    KIDT_CNT = KIDF_CNT + KIDM_CNT,
    KIDT_ECNT = KIDF_ECNT + KIDM_ECNT,
    LEUT_RATE = (LEUF_RATE+LEUM_RATE)/2,
    LEUT_CNT = LEUF_CNT + LEUM_CNT,
    LEUT_ECNT = LEUF_ECNT + LEUM_ECNT,
    LGBT_RATE = (LGBF_RATE+LGBM_RATE)/2,
    LGBT_CNT = LGBF_CNT + LGBM_CNT,
    LGBT_ECNT = LGBF_ECNT + LGBM_ECNT,
    LUNT_RATE = (LUNF_RATE+LUNM_RATE)/2,
    LUNT_CNT = LUNF_CNT + LUNM_CNT,
    LUNT_ECNT = LUNF_ECNT + LUNM_ECNT,
    NHLT_RATE = (NHLF_RATE+NHLM_RATE)/2,
    NHLT_CNT = NHLF_CNT + NHLM_CNT,
    NHLT_ECNT = NHLF_ECNT + NHLM_ECNT,
    ORAT_RATE = (ORAF_RATE+ORAM_RATE)/2,
    ORAT_CNT = ORAF_CNT + ORAM_CNT,
    ORAT_ECNT = ORAF_ECNT + ORAM_ECNT,
    OTHT_RATE = (OTHF_RATE+OTHM_RATE)/2,
    OTHT_CNT = OTHF_CNT + OTHM_CNT,
    OTHT_ECNT = OTHF_ECNT + OTHM_ECNT,
    OVAT_RATE = OVAF_RATE,
    OVAT_CNT = OVAF_CNT,
    OVAT_ECNT = OVAF_ECNT,
    PANT_RATE = (PANF_RATE+PANM_RATE)/2,
    PANT_CNT = PANF_CNT + PANM_CNT,
    PANT_ECNT = PANF_ECNT + PANM_ECNT,
    PROT_RATE = PROM_RATE,
    PROT_CNT = PROM_CNT,
    PROT_ECNT = PROM_ECNT,
    RECT_RATE = (RECF_RATE+RECM_RATE)/2,
    RECT_CNT = RECF_CNT + RECM_CNT,
    RECT_ECNT = RECF_ECNT + RECM_ECNT,
    STOT_RATE = (STOF_RATE+STOM_RATE)/2,
    STOT_CNT = STOF_CNT + STOM_CNT,
    STOT_ECNT = STOF_ECNT + STOM_ECNT,
    UTET_RATE = UTEF_RATE,
    UTET_CNT = UTEF_CNT,
    UTET_ECNT = UTEF_ECNT
  )

# add in the full state name, in case needed with some mapping libraries
StateSummT <- mutate(StateSummT,REGION=stateFromLower(StateSummT$ABBREV))

# # export the state totals with M and F stats
write.csv(StateSummT,'./ccm_statesummt.csv',row.names=FALSE)

# get key stats here for --------------------------------------------------------------
# including in my summary presentation slides

# US death counts
# How many people in US died in 15 years?
USCount <- sum(StateSummT$ACCT_CNT)
# Per Year?
sum(StateSummT$ACCT_CNT/15)
# Per Day?
sum(StateSummT$ACCT_CNT/15/365)
# Per Minute
sum(StateSummT$ACCT_CNT/15/365/24/60)

# US death rates
USRate <- mean(StateSummT$ACCT_RATE)
# in percent
cat("US Death Rate =",USRate," (or",round(USRate/100000*100,digits=2),"%)")

# states death counts
TopStatesCount <- select(arrange(StateSummT,-ACCT_CNT),ABBREV,ACCT_CNT)

# state death rates
TopStatesRate <- select(arrange(StateSummT,-ACCT_RATE),ABBREV,ACCT_RATE)

# draw the maps --------------------------------------------------------------------------

# plot the choropleth map for all cancer counts for each state

# data(stateMapEnv)
# shades <- colorRampPalette(c("white", "darkgreen"))(100)
# fills <- shades[as.integer(StateSummT$ACCT_CNT)]
# map('state', fill = TRUE, col=fills, resolution = 0, lty = 0,
#    projection = "polyconic", myborder = 0, mar = c(0,0,0,0))

library(maps)
library(mapproj)

# generate vector of fill colors for map
# map function had issues not recognizing the ACCT_CNT column metrics
shades <- colorRampPalette(c("white", "darkgreen"))(100)
fills <- shades[as.integer(StateSummT$ACCT_CNT)]
percent_map(StateSummT$ACCT_CNT, "darkgreen", "Total Deaths")

# plot choropleth map
map("state", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder=0, mar = c(0,0,0,0))

# overlay state borders - WORKS
map("state", col = "darkgray", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))

# add a legend - WORKS
legend.text <- c(paste0(0, " % or less"),
                 paste0(0 + 25, " %"),
                 paste0(0 + 2 * 25, " %"),
                 paste0(0 + 3 * 25, " %"),
                 paste0(100, " % or more"))

legend("bottomleft", # WORKS
       legend = legend.text,
       fill = shades[c(1, 25, 50, 75, 100)], 
       title = "Total Deaths")

# map function guide -----------------------------
# map(database = "world", regions = ".", exact = FALSE, boundary = TRUE,
#     interior = TRUE, projection = "", parameters = NULL, orientation = NULL,
#     fill = FALSE, col = 1, plot = TRUE, add = FALSE, namesonly = FALSE,
#     xlim = NULL, ylim = NULL, wrap = FALSE, resolution = if (plot) 1 else 0,
#     type = "l", bg = par("bg"), mar = c(4.1, 4.1, par("mar")[3], 0.1),
#     myborder = 0.01, namefield="name", ...)

