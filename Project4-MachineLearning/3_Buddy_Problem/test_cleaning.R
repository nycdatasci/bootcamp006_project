#Linlin Cheng
#cleaning for test.csv

#############DATA CLEANING for the TRUE Test set#####################
###############################################
    

########################################################
###correponding datasets are: 
#Full Test dataset : dfnew
#PRI_jet_num             0           1         2|3
#   w/ NAs             dfnew0     dfnew1      dfnew23
#   w/o NAs(imputed)   dfnew0_im  dfnew1_im   dfnew23_im
#######################################################
#######################################################

library(dplyr)
library(Hmisc) #missingness imputation
library(caret)

set.seed(0)
###
###SET IT TO YOUR OWN DIRECTORY!!!!

#setwd("~/Downloads/Kaggle_Jumpstart")

#load test dataset
dfnew <- read.csv("test.csv", header=T)  #size : 550000, 31
dfnew[dfnew==-999.0] <- NA

#split into four groups for PRI_jet_num=0, 1, 2, 3
#drop corresponding columns where missingness = 100%
#use random imputation for DER_mass_MMC, DER_prodeta_jet_jet
#and PRI_jet_subleading_phi


#table for Percentages of Missingness for PRI_jet_num =0

# 2                 DER_mass_MMC  57444 220156 0.2609241
# 6         DER_deltaeta_jet_jet 220156 220156 1.0000000
# 7             DER_mass_jet_jet 220156 220156 1.0000000
# 8          DER_prodeta_jet_jet 220156 220156 1.0000000
# 14      DER_lep_eta_centrality 220156 220156 1.0000000
# 25          PRI_jet_leading_pt 220156 220156 1.0000000
# 26         PRI_jet_leading_eta 220156 220156 1.0000000
# 27         PRI_jet_leading_phi 220156 220156 1.0000000
# 28       PRI_jet_subleading_pt 220156 220156 1.0000000
# 29      PRI_jet_subleading_eta 220156 220156 1.0000000
# 30      PRI_jet_subleading_phi 220156 220156 1.0000000


dfnew0<-filter(dfnew, PRI_jet_num==0)[,-c(6:8,14, 25:30)] 
 #2                 DER_mass_MMC 57444 220156 0.2609241 (only column with missingness)
dfnew0_im<-dfnew0
dfnew0_im$DER_mass_MMC = impute(dfnew0$DER_mass_MMC, "random")

#table for Percentages of Missingness for PRI_jet_num =1
# 2                 DER_mass_MMC  16713 169716 0.09847628
# 6         DER_deltaeta_jet_jet 169716 169716 1.00000000
# 7             DER_mass_jet_jet 169716 169716 1.00000000
# 8          DER_prodeta_jet_jet 169716 169716 1.00000000
# 14      DER_lep_eta_centrality 169716 169716 1.00000000
# 28       PRI_jet_subleading_pt 169716 169716 1.00000000
# 29      PRI_jet_subleading_eta 169716 169716 1.00000000
# 30      PRI_jet_subleading_phi 169716 169716 1.00000000


dfnew1<-filter(dfnew, PRI_jet_num==1)[, -c(6:8, 14, 28:30)] 
dfnew1_im<-dfnew1
dfnew1_im$DER_mass_MMC = impute(dfnew1$DER_mass_MMC, "random")



#table for Percentages of Missingness for PRI_jet_num =2|3
#2                 DER_mass_MMC  9665 160128 0.06035796

dfnew23<-filter(dfnew, PRI_jet_num==2| PRI_jet_num==3) 
dfnew23_im<-dfnew23
dfnew23_im$DER_mass_MMC = impute(dfnew23$DER_mass_MMC, "random")

############   
##END HERE    
#########################################
#########################################


