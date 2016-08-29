library(dplyr)
# read data and mark 999.0 as NAs
dfTrain <- read.csv('./data/training.csv', header=T)
dfTest <- read.csv('./data/test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA

weight <- dfTrain$Weight
labels <- dfTrain$Label

# remove varaibles weight, labels
train <- dfTrain[, -c(32,33)]
test <- dfTest

# merge data from train and test to a compelete dataset
dfBind <- rbind(train, test)

# check missingnesses 
library(VIM)
aggr(dfBind)

colnames(dfBind)[colSums(is.na(dfBind)) > 0] # Get colnames of missingnesses
# 11 varibles with missing values:
#[1] "DER_mass_MMC"           "DER_deltaeta_jet_jet"   "DER_mass_jet_jet"      
#[4] "DER_prodeta_jet_jet"    "DER_lep_eta_centrality" "PRI_jet_leading_pt"    
#[7] "PRI_jet_leading_eta"    "PRI_jet_leading_phi"    "PRI_jet_subleading_pt" 
#[10] "PRI_jet_subleading_eta" "PRI_jet_subleading_phi"

#remVar = paste0("-DER_mass_MMC", "-DER_deltaeta_jet_jet", "-DER_mass_jet_jet",      
#                "-DER_prodeta_jet_jet", "-DER_lep_eta_centrality", "-PRI_jet_leading_pt",    
#                "-PRI_jet_leading_eta", "-PRI_jet_leading_phi", "-PRI_jet_subleading_pt", 
#                "-PRI_jet_subleading_eta", "-PRI_jet_subleading_phi")

################## GLM Imputation ########################
### [1] "DER_mass_MMC"  ###
miss1_train <- filter(dfBind, !is.na(DER_mass_MMC))
miss1_pred <- filter(dfBind, is.na(DER_mass_MMC))

miss1.model = lm(DER_mass_MMC ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                    -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                    -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                    -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss1_train)
summary(miss1.model)

miss1_fill = predict(miss1.model, newdata = miss1_pred, interval = 'none')
miss1_fill_df = as.data.frame(miss1_fill)
miss1_fill_df$EventId = miss1_pred$EventId
# combine predicted data and original data
miss1_complete = rbind(miss1_fill_df, rename(select(miss1_train, c(EventId, DER_mass_MMC), miss1_fill = DER_mass_MMC)))
                       


### [2] "DER_deltaeta_jet_jet" ###
miss2_train <- filter(dfBind, !is.na(DER_deltaeta_jet_jet))
miss2_pred <- filter(dfBind, is.na(DER_deltaeta_jet_jet))

miss2.model = lm(DER_deltaeta_jet_jet ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                 -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                 -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                 -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss2_train)
summary(miss2.model)

miss2_fill = predict(miss2.model, newdata = miss2_pred, interval = 'none')
miss2_fill_df = as.data.frame(miss2_fill)
miss2_fill_df$EventId = miss2_pred$EventId
# combine predicted data and original data
miss2_complete = rbind(miss2_fill_df, rename(select(miss2_train, c(EventId, DER_deltaeta_jet_jet), miss2_fill = DER_deltaeta_jet_jet)))



### [3] "DER_mass_jet_jet" ###
miss3_train <- filter(dfBind, !is.na(DER_mass_jet_jet))
miss3_pred <- filter(dfBind, is.na(DER_mass_jet_jet))
 
miss3.model = lm(DER_mass_jet_jet ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                 -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                 -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                 -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss3_train)
summary(miss3.model)

miss3_fill = predict(miss3.model, newdata = select(miss3_pred, -DER_mass_jet_jet), interval = 'none')
miss3_fill_df = as.data.frame(miss3_fill)
miss3_fill_df$EventId = miss3_pred$EventId
# combine predicted data and original data
miss3_complete = rbind(miss3_fill_df, rename(select(miss3_train, c(EventId, DER_mass_jet_jet), miss3_fill = DER_mass_jet_jet)))


### [4] "DER_prodeta_jet_jet"  ###
miss4_train <- filter(dfBind, !is.na(DER_prodeta_jet_jet))
miss4_pred <- filter(dfBind, is.na(DER_prodeta_jet_jet))

miss4.model = lm(DER_prodeta_jet_jet ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                 -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                 -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                 -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss4_train)
summary(miss4.model)

miss4_fill = predict(miss4.model, newdata = miss4_pred, interval = 'none')
miss4_fill_df = as.data.frame(miss4_fill)
miss4_fill_df$EventId = miss4_pred$EventId

# combine predicted data and original data
miss4_complete = rbind(miss4_fill_df, rename(select(miss4_train, c(EventId, DER_prodeta_jet_jet), miss4_fill = DER_prodeta_jet_jet)))

### [5] "DER_lep_eta_centrality" ###
miss5_train <- filter(dfBind, !is.na(DER_lep_eta_centrality))
miss5_pred <- filter(dfBind, is.na(DER_lep_eta_centrality))

miss5.model = lm(DER_lep_eta_centrality ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                 -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                 -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                 -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss5_train)
summary(miss5.model)

miss5_fill = predict(miss5.model, newdata = miss5_pred, interval = 'none')
miss5_fill_df = as.data.frame(miss5_fill)
miss5_fill_df$EventId = miss5_pred$EventId

# combine predicted data and original data
miss5_complete = rbind(miss5_fill_df, rename(select(miss5_train, c(EventId, DER_lep_eta_centrality), miss5_fill = DER_lep_eta_centrality)))


### [6] "PRI_jet_leading_pt"  ###
miss6_train <- filter(dfBind, !is.na(PRI_jet_leading_pt))
miss6_pred <- filter(dfBind, is.na(PRI_jet_leading_pt))

miss6.model = lm(PRI_jet_leading_pt ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                 -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                 -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                 -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss6_train)
summary(miss6.model)

miss6_fill = predict(miss6.model, newdata = miss6_pred, interval = 'none')
miss6_fill_df = as.data.frame(miss6_fill)
miss6_fill_df$EventId = miss6_pred$EventId

# combine predicted data and original data
miss6_complete = rbind(miss6_fill_df, rename(select(miss6_train, c(EventId, PRI_jet_leading_pt), miss6_fill = PRI_jet_leading_pt)))


### [7] "PRI_jet_leading_eta" ###
miss7_train <- filter(dfBind, !is.na(PRI_jet_leading_eta))
miss7_pred <- filter(dfBind, is.na(PRI_jet_leading_eta))

miss7.model = lm(PRI_jet_leading_eta ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                 -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                 -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                 -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss7_train)
summary(miss7.model)

miss7_fill = predict(miss7.model, newdata = miss7_pred, interval = 'none')
miss7_fill_df = as.data.frame(miss7_fill)
miss7_fill_df$EventId = miss7_pred$EventId

# combine predicted data and original data
miss7_complete = rbind(miss7_fill_df, rename(select(miss7_train, c(EventId, PRI_jet_leading_eta), miss7_fill = PRI_jet_leading_eta)))


### [8] "PRI_jet_leading_phi"  ###
miss8_train <- filter(dfBind, !is.na(PRI_jet_leading_phi))
miss8_pred <- filter(dfBind, is.na(PRI_jet_leading_phi))

miss8.model = lm(PRI_jet_leading_phi ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                 -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                 -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                 -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss8_train)
summary(miss8.model)

miss8_fill = predict(miss8.model, newdata = miss8_pred, interval = 'none')
miss8_fill_df = as.data.frame(miss8_fill)
miss8_fill_df$EventId = miss8_pred$EventId

# combine predicted data and original data
miss8_complete = rbind(miss8_fill_df, rename(select(miss8_train, c(EventId, PRI_jet_leading_phi), miss8_fill = PRI_jet_leading_phi)))


### [9] "PRI_jet_subleading_pt" ###
miss9_train <- filter(dfBind, !is.na(PRI_jet_subleading_pt))
miss9_pred <- filter(dfBind, is.na(PRI_jet_subleading_pt))

miss9.model = lm(PRI_jet_subleading_pt ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                 -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                 -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                 -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss9_train)
summary(miss9.model)

miss9_fill = predict(miss9.model, newdata = miss9_pred, interval = 'none')
miss9_fill_df = as.data.frame(miss9_fill)
miss9_fill_df$EventId = miss9_pred$EventId

# combine predicted data and original data
miss9_complete = rbind(miss9_fill_df, rename(select(miss9_train, c(EventId, PRI_jet_subleading_pt), miss9_fill = PRI_jet_subleading_pt)))


### [10] "PRI_jet_subleading_eta" ###
miss10_train <- filter(dfBind, !is.na(PRI_jet_subleading_eta))
miss10_pred <- filter(dfBind, is.na(PRI_jet_subleading_eta))

miss10.model = lm(PRI_jet_subleading_eta ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                  -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                  -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                  -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss10_train)
summary(miss10.model)

miss10_fill = predict(miss10.model, newdata = miss10_pred, interval = 'none')
miss10_fill_df = as.data.frame(miss10_fill)
miss10_fill_df$EventId = miss10_pred$EventId

# combine predicted data and original data
miss10_complete = rbind(miss10_fill_df, rename(select(miss10_train, c(EventId, PRI_jet_subleading_eta), miss10_fill = PRI_jet_subleading_eta)))

### [11] "PRI_jet_subleading_phi" ###
miss11_train <- filter(dfBind, !is.na(PRI_jet_subleading_phi))
miss11_pred <- filter(dfBind, is.na(PRI_jet_subleading_phi))

miss11.model = lm(PRI_jet_subleading_phi ~ .-EventId -DER_mass_MMC -DER_deltaeta_jet_jet -DER_mass_jet_jet
                  -DER_prodeta_jet_jet -DER_lep_eta_centrality -PRI_jet_leading_pt    
                  -PRI_jet_leading_eta -PRI_jet_leading_phi -PRI_jet_subleading_pt
                  -PRI_jet_subleading_eta -PRI_jet_subleading_phi, data = miss11_train)
summary(miss11.model)

miss11_fill = predict(miss11.model, newdata = miss11_pred, interval = 'none')
miss11_fill_df = as.data.frame(miss11_fill)
miss11_fill_df$EventId = miss11_pred$EventId

# combine predicted data and original data
miss11_complete = rbind(miss11_fill_df, rename(select(miss11_train, c(EventId, PRI_jet_subleading_phi), miss11_fill = PRI_jet_subleading_phi)))

missingness_list <- list(miss1_complete, miss2_complete, miss3_complete, miss4_complete, miss5_complete, 
                              miss6_complete, miss7_complete, miss8_complete, miss9_complete, miss10_complete, 
                              miss11_complete)

# Combine all missingness together and save into .csv file
missingness_complete <- Reduce(function(x, y)merge(x, y, by = 'EventId'), missingness_list)
write.csv(missingness_complete, file = 'missingness_complete.csv')
