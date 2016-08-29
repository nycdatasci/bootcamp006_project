training<-read.csv('/Users/ikovsky/python_scripts/HiggsBoson/training.csv')
test<-read.csv('/Users/ikovsky/python_scripts/HiggsBoson/test.csv')
atlas<-read.csv('/Users/ikovsky/python_scripts/HiggsBoson/atlas-higgs-challenge-2014-v2.csv')

library(dplyr)

training2 <- training
labels    <- training$Label
weight <- training$Weight

training2$Label<-NULL
training2$EventId<-NULL
training2[training2 < -998]<-NA
training2$Weight<-NULL

test.atlas<-atlas[atlas$KaggleSet %in% c('b','v'),]
#X<-test.atlas %>% group_by(DER_mass_transverse_met_lep,DER_mass_vis,Label) %>% summarise(prob=n()/550000)
# > head(X)

X<-test.atlas %>% group_by(met=floor(DER_mass_transverse_met_lep),vis=floor(DER_mass_vis),Label) %>% summarise(prob=n()/550000)
# > dim(X)
# [1] 549988      4
X<-X[X$vis<300,]
X<-X[X$met<200,]

X_b<-X[X$Label=='b',]
X_s<-X[X$Label=='s',]
library(reshape2)
X_s$Label<-NULL
X_b$Label<-NULL
X_b <- data.matrix(dcast(X_b,met~vis))
X_s <- data.matrix(dcast(X_s,met~vis))
X_b[is.na(X_b)]<-0
X_s[is.na(X_s)]<-0
rownames(X_s)<-X_s[,1]
rownames(X_b)<-X_b[,1]
X_b<-X_b[,-1]
X_s<-X_s[,-1]
colnames(X_b)
colnames(X_s)

X_bn <- data.matrix(matrix(1:(200*300),nrow=200)) * 0
rownames(X_bn) <- paste(1:200)
colnames(X_bn) <- paste(1:300)

X_sn <- data.matrix(matrix(1:(200*300),nrow=200)) * 0
rownames(X_sn) <- paste(1:200)
colnames(X_sn) <- paste(1:300)
CS<-colnames(X_s)
CB<-colnames(X_b)

for (j in 1:300) {
  J<-toString(j)
  if (J %in% CS)  {X_sn[,J] = X_s[,J]}
  if (J %in% CB)  {X_bn[,J] = X_b[,J]}
} 

Q = 1:200
Q[Q%%20!=0]<-NA
R = 1:300
R[R%%30!=0]<-NA
heatmap(X_bn, Rowv=NA,Colv='Rowv',scale='none', labRow=Q, labCol=R,
        main='Test Set Background Prob',xlab='mass_vis',ylab='mass_transverse_met_lep')
heatmap(X_sn, Rowv=NA,Colv='Rowv',scale='none', labRow=Q, labCol=R,
        main='Test Set Signal Prob',xlab='mass_vis',ylab='mass_transverse_met_lep')

Y <-training %>% group_by(met=floor(DER_mass_transverse_met_lep),vis=floor(DER_mass_vis),Label) %>% summarise(prob=n()/250000)
# > dim(X)
# [1] 549988      4
Y<-Y[Y$vis<300,]
Y<-Y[Y$met<200,]


Y_b<-Y[Y$Label=='b',]
Y_s<-Y[Y$Label=='s',]

Y_s$Label<-NULL
Y_b$Label<-NULL
Y_b <- data.matrix(dcast(Y_b,met~vis,value.var='prob'))
Y_s <- data.matrix(dcast(Y_s,met~vis,value.var='prob'))
Y_b[is.na(Y_b)]<-0
Y_s[is.na(Y_s)]<-0
rownames(Y_s)<-Y_s[,1]
rownames(Y_b)<-Y_b[,1]
Y_b<-Y_b[,-1]
Y_s<-Y_s[,-1]
colnames(Y_b)
colnames(Y_s)

Y_bn <- data.matrix(matrix(1:(200*300),nrow=200)) * 0
rownames(Y_bn) <- paste(1:200)
colnames(Y_bn) <- paste(1:300)

Y_sn <- data.matrix(matrix(1:(200*300),nrow=200)) * 0
rownames(Y_sn) <- paste(1:200)
colnames(Y_sn) <- paste(1:300)
CS<-colnames(Y_s)
CB<-colnames(Y_b)

for (j in 1:300) {
  J<-toString(j)
  if (J %in% CS)  {Y_sn[1:199,J] = Y_s[,J]}
  if (J %in% CB)  {Y_bn[,J] = Y_b[,J]}
} 

heatmap(Y_bn, Rowv=NA,Colv='Rowv',scale='none', labRow=Q, labCol=R,
        main='Train Set Background Prob',xlab='mass_vis',ylab='mass_transverse_met_lep')
heatmap(Y_sn, Rowv=NA,Colv='Rowv',scale='none', labRow=Q, labCol=R,
        main='Train Set Signal Prob',xlab='mass_vis',ylab='mass_transverse_met_lep')


sum((abs(X_sn-Y_sn)>0.5*Y_sn)&(Y_sn>0))
#7251
plot_ly(z = X_sn-Y_sn, type = "surface")
plot_ly(z = X_bn-Y_bn, type = "surface")
plot_ly(z=X_sn,type='surface')
plot_ly(z=Y_sn,type='surface')
plot_ly(z=Y_bn,type='surface')

plot_ly(z=X_sn+X_bn-Y_sn-Y_bn,type='surface')

rf.massPair.TrainPred <- predict(m_rf.mass, newdata = test.pair, type="prob")
labels.train <- test.atlas.Label
auc<-roc(labels,rf.massPair.TrainPred[,2])
plot(auc,print.thres=TRUE)
pred<-ifelse(rf.massPair.TrainPred[,2]>0.456,'s','b')
table(truth=labels.train,pred=pred)

train.pair<-training[,3:4]
labels.test <- training.Label
rf.massPair.TestPred <- predict(m_rf.mass, newdata = train.pair, type="prob")
auc<-roc(labels.test,rf.massPair.TestPred[,2])
plot(auc,print.thres=TRUE)


pred<-ifelse(rf.massPair.TrainPred[,2]>0.343,'s','b')
table(truth=labels.Train,pred=pred)


##########################

Tau.Lep.Phi.test <- test.atlas %>% group_by(lep=floor(PRI_lep_phi*10),
                                            tau=floor(PRI_tau_phi*10),Label) %>% summarise(prob=n()/550000)

Tau.Lep.Phi.training <- training %>% group_by(lep=floor(PRI_lep_phi*10),
                                              tau=floor(PRI_tau_phi*10),Label) %>% summarise(prob=n()/250000)

Tau.Lep.Phi.test_b<-Tau.Lep.Phi.test[Tau.Lep.Phi.test$Label=='b',]
Tau.Lep.Phi.test_s<-Tau.Lep.Phi.test[Tau.Lep.Phi.test$Label=='s',]
Tau.Lep.Phi.training_b<-Tau.Lep.Phi.training[Tau.Lep.Phi.training$Label=='b',]
Tau.Lep.Phi.training_s<-Tau.Lep.Phi.training[Tau.Lep.Phi.training$Label=='s',]

Tau.Lep.Phi.test_b$Label<-NULL
Tau.Lep.Phi.test_s$Label<-NULL
Tau.Lep.Phi.training_b$Label<-NULL
Tau.Lep.Phi.training_s$Label<-NULL


Phi_Phi_testb <- data.matrix(dcast(Tau.Lep.Phi.test_b,lep~tau,value.var='prob'))
Phi_Phi_trainb <- data.matrix(dcast(Tau.Lep.Phi.training_b,lep~tau,value.var='prob'))
Phi_Phi_tests <- data.matrix(dcast(Tau.Lep.Phi.test_s,lep~tau,value.var='prob'))
Phi_Phi_trains <- data.matrix(dcast(Tau.Lep.Phi.training_s,lep~tau,value.var='prob'))

rownames(Phi_Phi_testb)<-Phi_Phi_testb[,1]*0.1
Phi_Phi_testb<-Phi_Phi_testb[,-1]

heatmap(Phi_Phi_testb, Rowv=NA,Colv='Rowv',scale='none',
        main='Test Set Background Phi-Phi Prob',xlab='tau',ylab='lep')

rownames(Phi_Phi_trainb)<-Phi_Phi_trainb[,1]*0.1
Phi_Phi_trainb<-Phi_Phi_trainb[,-1]

heatmap(Phi_Phi_trainb, Rowv=NA,Colv='Rowv',scale='none',
        main='Train Set Background Phi-Phi Prob',xlab='tau',ylab='lep')

rownames(Phi_Phi_tests)<-Phi_Phi_tests[,1]*0.1
Phi_Phi_tests<-Phi_Phi_tests[,-1]

heatmap(Phi_Phi_tests, Rowv=NA,Colv='Rowv',scale='none',
        main='Test Set Signal Phi-Phi Prob',xlab='tau',ylab='lep')

rownames(Phi_Phi_trains) <- Phi_Phi_trains[,1] * 0.1
Phi_Phi_trains<-Phi_Phi_trains[,-1]

Phi_Phi_trains[is.na(Phi_Phi_trains)]<-0

heatmap(Phi_Phi_trains, Rowv=NA, Colv='Rowv', scale='none',
        main='Train Set Signal Phi-Phi Prob',xlab='tau',ylab='lep')



#######################################
library(dplyr)
library(reshape2)

Eta_Eta.test <- test.atlas %>% group_by(lep=0.1*floor(PRI_lep_eta*10),
                                        tau=0.1*floor(PRI_tau_eta*10),Label) %>% summarise(prob=n()/550000)

Eta_Eta.training <- training %>% group_by(lep=0.1*floor(PRI_lep_eta*10),
                                          tau=0.1*floor(PRI_tau_eta*10),Label) %>% summarise(prob=n()/250000)

Eta_Eta.training_b <- Eta_Eta.training[Eta_Eta.training$Label=='b',]
Eta_Eta.training_s <- Eta_Eta.training[Eta_Eta.training$Label=='s',]

Eta_Eta.test_b <- Eta_Eta.test[Eta_Eta.test$Label=='b',]
Eta_Eta.test_s <- Eta_Eta.test[Eta_Eta.test$Label=='s',]


EE.tr_s<-data.matrix(dcast(Eta_Eta.training_s,lep~tau,value.var='prob'))
rownames(EE.tr_s)<-(EE.tr_s[,1])
EE.tr_s<-EE.tr_s[,-1]
EE.tr_s[is.na(EE.tr_s)]<-0
heatmap(EE.tr_s, Rowv=NA, Colv='Rowv', scale='none',
        main='Train Set Signal Lep-Tau Eta Eta Prob',xlab='tau',ylab='lep')


EE.tr_b<-data.matrix(dcast(Eta_Eta.training_b,lep~tau,value.var='prob'))
rownames(EE.tr_b)<-(EE.tr_b[,1])
EE.tr_b<-EE.tr_b[,-1]
EE.tr_b[is.na(EE.tr_b)]<-0
heatmap(EE.tr_b, Rowv=NA, Colv='Rowv', scale='none',
        main='Train Set Background Lep-Tau Eta Eta Prob',xlab='tau',ylab='lep')

EE.te_s<-data.matrix(dcast(Eta_Eta.test_s,lep~tau,value.var='prob'))
rownames(EE.te_s)<-(EE.te_s[,1])
EE.te_s<-EE.te_s[,-1]
EE.te_s[is.na(EE.te_s)] <- 0
heatmap(EE.te_s, Rowv=NA, Colv='Rowv', scale='none',
        main='Test Set Signal Lep-Tau Eta Eta Prob',xlab='tau',ylab='lep')

EE.te_b<-data.matrix(dcast(Eta_Eta.test_b,lep~tau,value.var='prob'))
rownames(EE.te_b)<-(EE.te_b[,1])
EE.te_b<-EE.te_b[,-1]
EE.te_b[is.na(EE.te_b)]<-0
heatmap(EE.te_b, Rowv=NA, Colv='Rowv', scale='none',
        main='Test Set Background Lep Phi- Tau Eta Prob',xlab='tau',ylab='lep')

############################
higgsMassProb <- training %>% group_by(higgsNA=DER_mass_MMC==-999) %>% summarise(sCount=sum(Label=='s'),count=n())
higgsMassProb$sampleProb<-round(higgsMassProb$sCount/higgsMassProb$count,digits=3)
#0.39092720 0.07438212

library(gridExtra)
grid.table(higgsMassProb)

#############################

prob_byJet<-training %>% group_by(PRI_jet_num) %>% summarise(prob=round(sum(Label=='s')/n(),3))
grid.table(prob_byJet)
#############################

source('Kaggle_Jumpstart/helper.R')

higgsCorr<-read.csv('HiggsCorr.csv')
rownames(higgsCorr)<-higgsCorr$X
higgsCorr$X<-NULL
higgsM<-data.matrix(higgsCorr)
diag(higgsM) <- 0
heatmap(higgsM, Rowv=NA,Colv='Rowv',scale='none')

#############################

higgsCorr<-read.csv('HiggsCorr.csv')
rownames(higgsCorr)<-higgsCorr$X
higgsCorr$X<-NULL
higgsM<-data.matrix(higgsCorr)
diag(higgsM) <- 0
heatmap(higgsM, Rowv=NA,Colv='Rowv',scale='none')


##############################

dist.higgs   <- dist(t(is.na(training2)))
fit.single   = hclust(dist.higgs, method = "single")
fit.complete = hclust(dist.higgs, method = "complete")
fit.average  = hclust(dist.higgs, method = "average")
# 
plot(fit.single, hang = -1, main = "Dendrogram of Single Linkage")
plot(fit.complete, hang = -1, main = "Dendrogram of Complete Linkage")
plot(fit.average, hang = -1, main = "Dendrogram of Average Linkage")

###############################

ImputeHiggsData<-function(data) {
  
  data$DER_mass_MMC[is.na(data$DER_mass_MMC)] <- 0.0
  noJet<- (data$PRI_jet_num   == 0)
  oneJet<- (data$PRI_jet_num  == 1)
  twoJets<- (data$PRI_jet_num == 2)
  data$DER_deltaeta_jet_jet[oneJet] <- abs(data$PRI_jet_leading_eta[oneJet])+20
  data$DER_deltaeta_jet_jet[noJet] <- 40.0
  data$DER_mass_jet_jet[noJet]   <- 0
  data$DER_mass_jet_jet[oneJet]  <- 0
  data$DER_prodeta_jet_jet[noJet] <- -400
  data$DER_prodeta_jet_jet[oneJet] <- -20*abs(data$PRI_jet_leading_eta[oneJet])
  
  data$DER_lep_eta_centrality[noJet]  <-    1.1
  data$DER_lep_eta_centrality[oneJet]  <-  exp(-1)         
  data$PRI_jet_leading_pt[noJet]      <- 0     
  data$PRI_jet_leading_eta[noJet]     <- 20        
  data$PRI_jet_leading_phi[noJet]     <- 4 
  data$PRI_jet_subleading_pt[noJet]   <- 0
  data$PRI_jet_subleading_pt[oneJet]  <- 0   
  data$PRI_jet_subleading_eta[noJet]  <- -20
  data$PRI_jet_subleading_eta[oneJet] <- -sign(data$PRI_jet_leading_eta[oneJet]) * 20    
  data$PRI_jet_subleading_phi[oneJet] <- 4    
  data$PRI_jet_subleading_phi[noJet]  <- 4 
  return(data)
}

##################################

library(caret)
library(doMC)
training2$Label<-NULL
training4<-ImputeHiggsData(training2)
training4$Label<-NULL
training4$EventId<-NULL
training4$Weight<-NULL
registerDoMC(cores = 6)

###################################

set.seed(0)
rfGrid <-  expand.grid(mtry = c(3,6,9))

ctrl = trainControl(method = "repeatedcv",number = 10,
                    summaryFunction = AMS_summary)

m_rf.imputed2 = train(x=training4, y=labels, 
                      method="rf", weights=weight, tuneGrid=rfGrid,
                      verbose=TRUE, trControl=ctrl, metric="AMS")

svmGrid<-expand.grid(sigma = c(.01, .015, 0.2),
                     C = c(0.75, 0.9, 1, 1.1, 1.25))

svmGrid<-expand.grid(C = c(10000))

ctrl = trainControl(method = "repeatedcv",number = 5,
                    summaryFunction = AMS_summary)

m_svmRadial.imputed2 <- train(x=training.scaled, y=labels, 
                              method="svmRadial", weights=weight, tuneGrid=svmGrid,
                              verbose=TRUE, trControl=ctrl, metric="AMS")
####################################

rf.imputed.TrainPred <- predict(m_rf.imputed2, newdata=training4, type="prob")

library(pROC)
labelA <- ifelse(labels=='s', 1, 0)
auc = roc(labelA,rf.imputed.TrainPred[,2])
plot(auc, print.thres=TRUE)

m_rf.imputed2 = train(x=X, y=labels, 
                      method="rf", weights=weight, tuneGrid=rfGrid,
                      verbose=TRUE, trControl=ctrl, metric="AMS")



predicted <- rep("b",550000)
predicted[rfTestPred[,2]>threshold] <- "s"
weightRank = rank(rfTestPred[,2], ties.method= "random")

submission = data.frame(EventId = test$EventId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "rf_submission.csv", row.names=FALSE)

########################################

p<-apply(is.na(training2),MARGIN=2,FUN=sum)
trainingNoNA<-training2[,p]


ctrl = trainControl(method = "repeatedcv",number = 6,
                    summaryFunction = AMS_summary)

m_rf.imputed2 = train(x=trainingNoNA, y=labels, 
                      method="rf", weights=weight,
                      verbose=TRUE, trControl=ctrl, metric="AMS")


ctrl = trainControl(method = "repeatedcv",number = 2,
                    summaryFunction = AMS_summary)

rfGrid <-  expand.grid(mtry = c(3))

m_rf.imputed_node     = train(x=training4, y=labels, 
                              method="rf", weights=weight, nodesize=200, tuneGrid=rfGrid,
                              verbose=TRUE, trControl=ctrl, metric="AMS")

rf.node.TrainPred <- predict(m_rf.imputed_node, newdata=training4, type="prob")

auc = roc(labelA,rf.node.TrainPred[,2])
plot(auc, print.thres=TRUE)

m_rf.imputed_node150     = train(x=training4, y=labels, 
                                 method="rf", weights=weight, nodesize=150, tuneGrid=rfGrid,
                                 verbose=TRUE, trControl=ctrl, metric="AMS")

m_rf.imputed_node50     = train(x=training4, y=labels, 
                                method="rf", weights=weight, nodesize=50, tuneGrid=rfGrid,
                                verbose=TRUE, trControl=ctrl, metric="AMS")

trainingOUT<-training4[,seq(-1,-3)]

m_rf.top3OUT           = train(x=trainingOUT, y=labels, 
                               method="rf", weights=weight, tuneGrid=rfGrid,
                               verbose=TRUE, trControl=ctrl, metric="AMS")





