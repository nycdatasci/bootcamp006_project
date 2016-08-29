dfTrain <- read.csv('./data/training.csv', header=T)
View(dfTrain)
unique(dfTrain$PRI_jet_num)
dfTest <- read.csv('./data/test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTrain$PRI_jet_num=as.factor(dfTrain$PRI_jet_num)
subset1=dfTrain[dfTrain$PRI_jet_num==0,]
nrow(subset1) #[1] 99913
which(apply(subset1, 2, function(x){any(is.na(x))}))
newsubset1=subset1[,-c(6,7,8,14,25, 26, 27, 28, 29, 30)]
View(newsubset1)
library(kknn)
#names(newsubset1)
#which(apply(newsubset1, 2, function(x){any(is.na(x))}))
newsubset2=newsubset1[,-c(1,22,23)]
complete = newsubset2[complete.cases(newsubset2),]
missing =newsubset2[!complete.cases(newsubset2),]
sqrt(nrow(newsubset2))
subset.euclidean = kknn(DER_mass_MMC ~ ., complete, missing, 
                        k = 316, distance = 2)
values =subset.euclidean$fitted.values
newsubset2[!complete.cases(newsubset2),]$DER_mass_MMC=values
#which(apply(newsubset2, 2, function(x){any(is.na(x))}))
#names(newsubset2)
finalsubset1=cbind(newsubset2,newsubset1[,c(1,22,23)])
write.csv(finalsubset1, file='subset0.csv')
#View(finalsubset1)


# DER_mass_MMC   DER_deltaeta_jet_jet       DER_mass_jet_jet 
# 2                      6                      7 
# DER_prodeta_jet_jet DER_lep_eta_centrality     PRI_jet_leading_pt 
# 8                     14                     25 
# PRI_jet_leading_eta    PRI_jet_leading_phi  PRI_jet_subleading_pt 
# 26                     27                     28 
# PRI_jet_subleading_eta PRI_jet_subleading_phi 
# 29                     30 

subset2=dfTrain[dfTrain$PRI_jet_num==1,]
#nrow(subset2) #77544
which(apply(subset2, 2, function(x){any(is.na(x))}))
df1=subset2[,-c(6,7, 8, 14, 28, 29, 30)]
#names(df1)
#View(df1)
df2=df1[,-c(1,25,26)]
complete1 = df2[complete.cases(df2),]
missing1 =df2[!complete.cases(df2),]
sqrt(nrow(df2))
subset.euclidean1 = kknn(DER_mass_MMC ~ ., complete1, missing1, 
                        k = 278, distance = 2)
values1=subset.euclidean1$fitted.values
df2[!complete.cases(df2),]$DER_mass_MMC=values1
finalsubset2=cbind(df2,df1[,c(1,25,26)])
#View(finalsubset2)
write.csv(finalsubset2, file='subset1.csv')


# DER_mass_MMC   DER_deltaeta_jet_jet       DER_mass_jet_jet 
# 2                      6                      7 
# DER_prodeta_jet_jet DER_lep_eta_centrality  PRI_jet_subleading_pt 
# 8                     14                     28 
# PRI_jet_subleading_eta PRI_jet_subleading_phi 
# 29                     30 
nrow(subset2) # [1] 77544
subset32=dfTrain[dfTrain$PRI_jet_num==2,]
subset33=dfTrain[dfTrain$PRI_jet_num==3,]
subset323=dfTrain[dfTrain$PRI_jet_num==3 |dfTrain$PRI_jet_num==2,]
subset3=rbind(subset32, subset33)
write.csv(subset3, file='newsubset23.csv')


