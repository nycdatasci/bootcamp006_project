library(dplyr)
library(Hmisc)
library(VIM)

#Reading in traing and testing data
train = read.csv('/Users/cholmes/Desktop/training.csv')
test = read.csv('/Users/cholmes/Desktop/test.csv')

#Some vectors to be used later, along with the option to replace -999's with NAs
labels = train$Label
weight = train$Weight
testId = test$EventId
#train[train == -999.000] = NA


#Selecting columns with missing data
selected_train = select(train, PRI_jet_num, DER_deltaeta_jet_jet, DER_mass_jet_jet, DER_prodeta_jet_jet, DER_lep_eta_centrality, PRI_jet_leading_pt, PRI_jet_leading_eta, PRI_jet_leading_phi, PRI_jet_subleading_pt, PRI_jet_subleading_eta, PRI_jet_subleading_phi,DER_mass_MMC)

missing_2 = select(train, PRI_jet_leading_pt, PRI_jet_leading_eta,PRI_jet_leading_phi)
missing_4 = select(train, DER_deltaeta_jet_jet,DER_mass_jet_jet,DER_prodeta_jet_jet,DER_lep_eta_centrality,PRI_jet_subleading_pt,PRI_jet_subleading_eta,PRI_jet_subleading_phi)

#Examining data broken out by PRI_jet_num
grouped_2 = summarise(group_by(train, PRI_jet_num), sum(is.na(PRI_jet_leading_pt))) #missing2 group
summarise(group_by(train, PRI_jet_num), sum(is.na(PRI_jet_leading_pt)-1)) #missing2 group
grouped_4 = summarise(group_by(train, PRI_jet_num), sum(is.na(DER_deltaeta_jet_jet))) #missing4 group
summarise(group_by(train, PRI_jet_num), sum(is.na(DER_deltaeta_jet_jet)-1)) #missing4 group
summarise(group_by(train, PRI_jet_num), mean(PRI_jet_leading_pt), mean(PRI_jet_leading_eta), mean(PRI_jet_leading_phi)) #missing2 group
summarise(group_by(train, PRI_jet_num), mean(DER_deltaeta_jet_jet), mean(DER_mass_jet_jet), mean(DER_prodeta_jet_jet),mean(DER_lep_eta_centrality),mean(PRI_jet_subleading_pt),mean(PRI_jet_subleading_eta),mean(PRI_jet_subleading_phi)) #missing4 group


#Creating training datasets
df_0 = filter(train, PRI_jet_num == 0)
df_1 = filter(train, PRI_jet_num == 1)
df_2 = filter(train, PRI_jet_num == 2)
df_3 = filter(train, PRI_jet_num == 3)

#Random Imputation for DER_mass_MMC
df_0['DER_mass_MMC'] = impute(df_0['DER_mass_MMC'], "random")
df_1['DER_mass_MMC'] = impute(df_1['DER_mass_MMC'], "random")
df_2['DER_mass_MMC'] = impute(df_2['DER_mass_MMC'], "random")
df_3['DER_mass_MMC'] = impute(df_3['DER_mass_MMC'], "random")

#Dropping missing columns for df_0 and df_1
drops_0 = c('DER_deltaeta_jet_jet','DER_mass_jet_jet','DER_prodeta_jet_jet','DER_lep_eta_centrality','PRI_jet_subleading_pt','PRI_jet_subleading_eta','PRI_jet_subleading_phi','PRI_jet_leading_pt', 'PRI_jet_leading_eta','PRI_jet_leading_phi')
drops_1 = c('DER_deltaeta_jet_jet','DER_mass_jet_jet','DER_prodeta_jet_jet','DER_lep_eta_centrality','PRI_jet_subleading_pt','PRI_jet_subleading_eta','PRI_jet_subleading_phi')

df_0 = df_0[ , !(names(df_0) %in% drops_0)]
df_1 = df_1[ , !(names(df_1) %in% drops_1)]

#Creating Testing Datasets
df_0_test = filter(test, PRI_jet_num == 0)
df_1_test = filter(test, PRI_jet_num == 1)
df_2_test = filter(test, PRI_jet_num == 2)
df_3_test = filter(test, PRI_jet_num == 3)

#Random imputation for testing datasets
df_0_test['DER_mass_MMC'] = impute(df_0_test['DER_mass_MMC'], "random")
df_1_test['DER_mass_MMC'] = impute(df_1_test['DER_mass_MMC'], "random")
df_2_test['DER_mass_MMC'] = impute(df_2_test['DER_mass_MMC'], "random")
df_3_test['DER_mass_MMC'] = impute(df_3_test['DER_mass_MMC'], "random")

drops_0 = c('DER_deltaeta_jet_jet','DER_mass_jet_jet','DER_prodeta_jet_jet','DER_lep_eta_centrality','PRI_jet_subleading_pt','PRI_jet_subleading_eta','PRI_jet_subleading_phi','PRI_jet_leading_pt', 'PRI_jet_leading_eta','PRI_jet_leading_phi')
drops_1 = c('DER_deltaeta_jet_jet','DER_mass_jet_jet','DER_prodeta_jet_jet','DER_lep_eta_centrality','PRI_jet_subleading_pt','PRI_jet_subleading_eta','PRI_jet_subleading_phi')

df_0_test = df_0_test[ , !(names(df_0_test) %in% drops_0)]
df_1_test = df_1_test[ , !(names(df_1_test) %in% drops_1)]

#Creating split vectors to be used later
testId_0 = df_0_test$EventId
testId_1 = df_1_test$EventId
testId_2 = df_2_test$EventId
testId_3 = df_3_test$EventId
weight_0 <- df_0$Weight
labels_0 <- df_0$Label
weight_1 <- df_1$Weight
labels_1 <- df_1$Label
weight_2 <- df_2$Weight
labels_2 <- df_2$Label
weight_3 <- df_3$Weight
labels_3 <- df_3$Label

#dropping unnecessary columns from dataframes
df_0 <- df_0[, -c(1,22,23)]
df_0_test <- df_0_test[,-1]
df_1 <- df_1[, -c(1,25,26)]
df_1_test <- df_1_test[,-1]
df_2 <- df_2[, -c(1,32,33)]
df_2_test <- df_2_test[,-1]
df_3 <- df_3[, -c(1,32,33)]
df_3_test <- df_3_test[,-1]


train <- train[, -c(1,32,33)]
test <- test[,-1]




