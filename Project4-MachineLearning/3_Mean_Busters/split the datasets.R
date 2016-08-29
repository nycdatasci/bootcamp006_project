setwd("~/Higgs Boson Kaggle Challenge")
dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)

#Split the Data
dfTrain.jet2=dfTrain[dfTrain$PRI_jet_num>=2,]
dfTrain.jet2=dfTrain.jet2[,-which(names(dfTrain.jet2) %in% c('PRI_jet_num'))]
dfTrain.remain=dfTrain[dfTrain$PRI_jet_num<2,]
dfTrain.remain=dfTrain.remain[,-which(names(dfTrain.remain) %in% c('DER_lep_eta_centrality',
                                                                   'DER_deltaeta_jet_jet',
                                                                   'DER_mass_jet_jet',
                                                                   'DER_prodeta_jet_jet',
                                                                   'PRI_jet_subleading_pt',
                                                                   'PRI_jet_subleading_eta',
                                                                   'PRI_jet_subleading_phi'))]

dfTrain.jet1=dfTrain.remain[dfTrain.remain$PRI_jet_num==1,]
dfTrain.jet1=dfTrain.jet1[,-which(names(dfTrain.jet1) %in% c('PRI_jet_num'))]
dfTrain.remain=dfTrain.remain[dfTrain.remain$PRI_jet_num!=1,]
dfTrain.jet0=dfTrain.remain[,-which(names(dfTrain.remain) %in% c('PRI_jet_leading_pt',
                                                                 'PRI_jet_leading_eta',
                                                                 'PRI_jet_leading_phi',
                                                                 'PRI_jet_num'))]


dfTest.jet2=dfTest[dfTest$PRI_jet_num>=2,]
dfTest.jet2=dfTest.jet2[,-which(names(dfTest.jet2) %in% c('PRI_jet_num'))]
dfTest.remain=dfTest[dfTest$PRI_jet_num<2,]
dfTest.remain=dfTest.remain[,-which(names(dfTest.remain) %in% c('DER_lep_eta_centrality',
                                                                'DER_deltaeta_jet_jet',
                                                                'DER_mass_jet_jet',
                                                                'DER_prodeta_jet_jet',
                                                                'PRI_jet_subleading_pt',
                                                                'PRI_jet_subleading_eta',
                                                                'PRI_jet_subleading_phi'))]
dfTest.jet1=dfTest.remain[dfTest.remain$PRI_jet_num==1,]
dfTest.jet1=dfTest.jet1[,-which(names(dfTest.jet1) %in% c('PRI_jet_num'))]
dfTest.remain=dfTest.remain[dfTest.remain$PRI_jet_num!=1,]
dfTest.jet0=dfTest.remain[,-which(names(dfTest.remain) %in% c('PRI_jet_leading_pt',
                                                              'PRI_jet_leading_eta',
                                                              'PRI_jet_leading_phi',
                                                              'PRI_jet_num'))]


write.csv(dfTrain.jet0, "dfTrain.jet0.csv", row.names=FALSE)
write.csv(dfTrain.jet1, "dfTrain.jet1.csv", row.names=FALSE)
write.csv(dfTrain.jet2, "dfTrain.jet2.csv", row.names=FALSE)

write.csv(dfTest.jet0, "dfTest.jet0.csv", row.names=FALSE)
write.csv(dfTest.jet1, "dfTest.jet1.csv", row.names=FALSE)
write.csv(dfTest.jet2, "dfTest.jet2.csv", row.names=FALSE)