library(dplyr)
library(VIM)
library(mice)
library(PASWR)

data = read.csv('./training.csv', header=T)
names(data)

EventId = data$EventId
Weight  = data$Weight
Label   = data$Label

data = data[, -c(1,32,33)]          # Get rid of EventId Weight Label
PRI_jet_num = data$PRI_jet_num      # Jet number is a category variable so donot scale
data = as.data.frame(scale(data))   # scale data , it scale jet number also 
data$PRI_jet_num = PRI_jet_num      # Unscale jet number back to its integer value

tf = colSums(is.na(data)) !=0       # Columns missing any data 

length(colnames(data)[tf])          # Number of columns 
#[1] 11

colSums(is.na(data))[colnames(data)[colSums(is.na(data)) !=0] ]/nrow(data)*100     # Column names and % of missing data 
#DER_mass_MMC   DER_deltaeta_jet_jet       DER_mass_jet_jet    DER_prodeta_jet_jet DER_lep_eta_centrality 
#15.2456                70.9828                70.9828                70.9828                70.9828 
#PRI_jet_leading_pt    PRI_jet_leading_eta    PRI_jet_leading_phi  PRI_jet_subleading_pt PRI_jet_subleading_eta 
#39.9652                39.9652                39.9652                70.9828                70.9828 
#PRI_jet_subleading_phi 
#70.9828

colnames(data)[which(colSums(is.na(data))[colnames(data)[colSums(is.na(data)) !=0] ]/nrow(data)*100 >70)] # Columns missing more than 70 % data
#[1] "DER_mass_transverse_met_lep" "DER_mass_vis"                "DER_pt_h"                    "DER_deltaeta_jet_jet"       
#[5] "DER_pt_tot"                  "DER_sum_pt"                  "DER_pt_ratio_lep_tau" 

sum(is.na(data))/ (nrow(data)* ncol(data))*100    # % of cells missing datasum(complete.cases(data)) / nrow(data)*100
#[1] 21.06736

sum(complete.cases(data)) / nrow(data)*100    # % of events/rows with complete data
#[1] 27.2456

aggr(data)
md.pattern(data)
