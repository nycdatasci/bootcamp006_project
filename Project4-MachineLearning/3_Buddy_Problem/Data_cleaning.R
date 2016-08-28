
#############DATA CLEANING#####################
###############################################


########################################################
###correponding datasets are: 
#  non-imputed full dataset:       df0, df1, df23
#  imputed full dataset:          df0_im, df1_im, df23_im
#  non_imputed train set:         df0_train, df1_train, df23_train
#  imputed train set:             df0_im_train, df1_im_train, df23_im_train
#  non_imputed test set:          df0_test, df1_test, df23_test 
#  imputed test set:              df0_im_test, df1_im_test, df23_im_test,
#######################################################
#######################################################

library(dplyr)
library(Hmisc) #missingness imputation
library(caret)

set.seed(0)
#setwd("~/Downloads/Kaggle_Jumpstart")

#load training dataset with size [250000, 33]
df <- read.csv("training.csv", header=T) 
df[df==-999.0] <- NA
df$Label<-ifelse(df$Label=='s', 1, 0)

#################################################
#delete outliers for max identified from boxplots:
#which.max(df$DER_pt_h) #7344
#which.max(df$DER_pt_tot) #7344
#which.max(df$PRI_tau_pt) #202796
#  which.max(df$PRI_tau_pt[-which.max(df$PRI_tau_pt)]) #191363
#which.max(df$PRI_met) #7344
#which.max(df$PRI_jet_leading_pt) #68118

df<-df[-c(7344, 202796, 191363, 68118),] 

#split into four groups for PRI_jet_num=0, 1, 2, 3
#drop corresponding columns where missingness = 100%
#use random imputation for DER_mass_MMC, DER_prodeta_jet_jet
#and PRI_jet_subleading_phi


#table for Percentages of Missingness for PRI_jet_num =0

#23                DER_mass_MMC 0.26    #**2
#24        DER_deltaeta_jet_jet    1    #6
#25            DER_mass_jet_jet    1    #7
#26         DER_prodeta_jet_jet    1    #8
#27      DER_lep_eta_centrality    1    #14
#28          PRI_jet_leading_pt    1    #25
#29         PRI_jet_leading_eta    1    #26
#30         PRI_jet_leading_phi    1    #27
#31       PRI_jet_subleading_pt    1    #28
#32      PRI_jet_subleading_eta    1    #29
#33      PRI_jet_subleading_phi    1    #30

df0<-filter(df, PRI_jet_num==0)[,-c(6:8,14, 25:30)] 
df0_im<-df0
df0_im$DER_mass_MMC = impute(df0$DER_mass_MMC, "random")
#splitting into 80:20 percent for training and test set:
train.index0 <- createDataPartition(df0$Label, p = .8, list = FALSE) 
#with missingness:
df0_train<-df0[train.index0, ]
df0_test<-df0[-train.index0,]
#with imputation:
df0_im_train<-df0_im[train.index0,]
df0_im_test<-df0_im[-train.index0,]


#table for Percentages of Missingness for PRI_jet_num =1
#  2                 DER_mass_MMC  7562 77544 0.09751883  #2
#  6         DER_deltaeta_jet_jet 77544 77544 1.00000000  #6
#  7             DER_mass_jet_jet 77544 77544 1.00000000  #7
#  8          DER_prodeta_jet_jet 77544 77544 1.00000000  #8
#  14      DER_lep_eta_centrality 77544 77544 1.00000000  #14
#  28       PRI_jet_subleading_pt 77544 77544 1.00000000  #28
#  29      PRI_jet_subleading_eta 77544 77544 1.00000000  #29
#  30      PRI_jet_subleading_phi 77544 77544 1.00000000  #30


df1<-filter(df, PRI_jet_num==1)[, -c(6:8, 14, 28:30)] 
df1_im<-df1
df1_im$DER_mass_MMC = impute(df1$DER_mass_MMC, "random")
#splitting into 80:20 percent for training and test set:
train.index1 <- createDataPartition(df1$Label, p = .8, list = FALSE) 
#with missingness:
df1_train<-df1[train.index1, ]
df1_test<-df1[-train.index1,]
#with imputation:
df1_im_train<-df1_im[train.index1,]
df1_im_test<-df1_im[-train.index1,]


#table for Percentages of Missingness for PRI_jet_num =2|3
#2                 DER_mass_MMC  4429 72543 0.06105344416

df23<-filter(df, PRI_jet_num==2| PRI_jet_num==3) 
df23_im<-df23
df23_im$DER_mass_MMC = impute(df23$DER_mass_MMC, "random")

#splitting into 80:20 percent for training and test set:
train.index23 <- createDataPartition(df23$Label, p = .8, list = FALSE) 
#with missingness:
df23_train<-df23[train.index23, ]
df23_test<-df23[-train.index23,]
#with imputation:
df23_im_train<-df23_im[train.index23,]
df23_im_test<-df23_im[-train.index23,]