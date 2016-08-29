setwd("~/Desktop/Kaggle_Jumpstart")
library(car)
library(psych)
library(FactoMineR)
library(plotly)
library("factoextra")
library(corrplot)
library(dplyr)
################################
# Pre-Processsing Data         #
################################
# Read data and mark 999.0 as NAs
dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA
testId = dfTest$EventId

str(dfTrain)

weight <- dfTrain$Weight # weight is a probability of the occurance of the observation in the real life
labels <- dfTrain$Label # response variable 

# Remove 1 = eventid , 32= weight, 33= label 
train <- dfTrain[, -c(1,32,33)] 
test <- dfTest[, -1] # Eventid

# Complete Case
sum(complete.cases(train)) #68114
train_c <- train[complete.cases(train),] # complete case of training data set without 1 = eventid , 32= weight, 33= label
#write.csv(train_c,file = "train_complete.csv",row.names = F)
###################################
# EDA  Analysis                 ###
###################################
#  correlation heat map 

cor.higgs<- round(cor(train_c),2)


corrplot(cor.higgs, type = "lower", order="hclust", 
         tl.col="lightblue", tl.srt=50,#mar = c(5,0,0,0),
         title = "Correlation Matrix of Higgs Boston Data")

#Visualize correlation matrix using a heatmap +hierarchical structure
col<- colorRampPalette(c("red","white", "blue"))(20)
heatmap(x = cor.higgs, col = col, symm = TRUE)

# histogram PRI_jet_num
hist(train$PRI_jet_num)

#EDA Missing Value Map
#http://www.njtierney.com/r/missing%20data/rbloggers/2015/12/01/ggplot-missing-data/
library(dplyr)
library(reshape2)
library(ggplot2)
ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

ggplot_missing(train)
# missing value only 
ggplot_missing(train[c(24,25,26,1,5,6,7,13,27,28,29)])
## Percentage of missing by columns
sum(is.na(train[1]))/nrow(train)  #"DER_mass_MMC"            0.152456 
sum(is.na(train[5]))/nrow(train)  #"DER_deltaeta_jet_jet"    0.709828
sum(is.na(train[6]))/nrow(train)  #"DER_mass_jet_jet"        0.709828
sum(is.na(train[7]))/nrow(train)  #"DER_prodeta_jet_jet"     0.709828
sum(is.na(train[13]))/nrow(train) #"DER_lep_eta_centrality"  0.709828
sum(is.na(train[24]))/nrow(train) #"PRI_jet_leading_pt"      0.399652
sum(is.na(train[25]))/nrow(train) #"PRI_jet_leading_eta"     0.399652
sum(is.na(train[26]))/nrow(train) #"PRI_jet_leading_phi"     0.399652
sum(is.na(train[27]))/nrow(train) #"PRI_jet_subleading_pt"   0.709828
sum(is.na(train[28]))/nrow(train) #"PRI_jet_subleading_eta"  0.709828
sum(is.na(train[29]))/nrow(train) #"PRI_jet_subleading_phi"  0.709828

############################################
##missing value of "DER_mass_MMC" by label## 
############################################



#############################################
#subset the dfTrian data by PRI_jet_num ###
#############################################
train_jet0<- dfTrain %>%
  filter(PRI_jet_num==0) %>%
  select(-EventId,-Weight,-Label,-PRI_jet_num,
         -DER_deltaeta_jet_jet,-DER_mass_jet_jet,-DER_prodeta_jet_jet,
         -DER_lep_eta_centrality, 
         -PRI_jet_leading_pt,-PRI_jet_leading_eta,-PRI_jet_leading_phi,
         -PRI_jet_subleading_pt,-PRI_jet_subleading_eta,-PRI_jet_subleading_phi,
         -PRI_jet_all_pt )
# keep the EventId,Weight,Label
train_jet0_track <- dfTrain %>%
  filter(PRI_jet_num==0)%>%
  select(EventId,Weight,Label)
#rownames(train_jet0) = 1:nrow(train_jet0)

train_jet1<- dfTrain %>%
  filter(PRI_jet_num==1) %>%
  select(-EventId,-Weight,-Label,-PRI_jet_num,
         -DER_deltaeta_jet_jet,-DER_mass_jet_jet,-DER_prodeta_jet_jet,
         -DER_lep_eta_centrality, 
         -PRI_jet_subleading_pt,-PRI_jet_subleading_eta,-PRI_jet_subleading_phi)
# keep the EventId,Weight,Label
train_jet1_track <- dfTrain %>%
  filter(PRI_jet_num==1)%>%
  select(EventId,Weight,Label)

#rownames(train_jet1) = 1:nrow(train_jet1)

train_jet2.3<- dfTrain %>%
  filter(PRI_jet_num==2 | PRI_jet_num==3) %>%
  select(-EventId,-Weight,-Label,-PRI_jet_num)

# keep the EventId,Weight,Label
train_jet2.3_track <- dfTrain %>%
  filter(PRI_jet_num==2 | PRI_jet_num==3)%>%
  select(EventId,Weight,Label)


train_jet2 <- subset(train,train$PRI_jet_num==2)
rownames(train_jet2) = 1:nrow(train_jet2)

rownames(train_jet2.3) = 1:nrow(train_jet2.3)
train_jet3 <- subset(train,train$PRI_jet_num==3)
rownames(train_jet3) = 1:nrow(train_jet3)

# missing value map 

ggplot_missing(train_jet0)+ggtitle("missing value heat map of train_jet0")
ggplot_missing(train_jet1)+ ggtitle("missing value heat map of train_jet1")
ggplot_missing(train_jet2.3) + ggtitle("missing value heat map of train_jet2.3")
ggplot_missing(train_jet2)+ ggtitle("missing value heat map of train_jet2")
ggplot_missing(train_jet3)+ ggtitle("missing value heat map of train_jet3")



#percentage
sum(is.na(train_jet0[1]))/nrow(train_jet0) # "DER_mass_MMC" 0.2614575
sum(is.na(train_jet1[1]))/nrow(train_jet1) # "DER_mass_MMC" 0.09751883
sum(is.na(train_jet2.3[1]))/nrow(train_jet2.3)  # "DER_mass_MMC" NA %  0.06105344




###########################################
# Visualization by Jet_2  / jet_3         #
###########################################
library(ggplot2)
train_factor <- dfTrain
train_factor$PRI_jet_num <- as.factor(train_factor$PRI_jet_num)

g_1<- ggplot(data=train_factor,aes(x=DER_mass_MMC,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_density(alpha = 0.3)+ggtitle("Distribution of Higgs_Boston By Label")+ 
  facet_wrap(~ Label)

plot(g_1)

g_2<- ggplot(data=train_factor)+
  geom_density(aes(x=DER_mass_transverse_met_lep,fill=PRI_jet_num,colour=PRI_jet_num), alpha = 0.3)+
  ggtitle("Distribution of Higgs_Boston By Label")+ 
  facet_wrap(~ Label)

plot(g_2)

g_3<- ggplot(data=train_factor,aes(x=DER_mass_vis,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_density(alpha = 0.3)+
  ggtitle("Distribution of Higgs_Boston By Label")+ 
  facet_wrap(~ Label)

plot(g_3) 

g_4<- ggplot(data=train_factor,aes(x=DER_pt_h,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_density(alpha = 0.3)+
  ggtitle("Distribution of Higgs_Boston By Label")+ 
  facet_wrap(~ Label)

plot(g_4) 

g_5<- ggplot(data=train_factor,aes(x=DER_deltaeta_jet_jet,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_density(alpha = 0.3)+
  ggtitle("Distribution of Higgs_Boston By Label")+ 
  facet_wrap(~ Label)

plot(g_5) 

g_6<- ggplot(data=train_factor,aes(x=DER_mass_jet_jet,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_density(alpha = 0.3)+
  ggtitle("Distribution of Higgs_Boston By Label")+ 
  facet_wrap(~ Label)

plot(g_6) 

g_7<- ggplot(data=train_factor,aes(x=DER_prodeta_jet_jet,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_density(alpha = 0.3)+
  ggtitle("Distribution of Higgs_Boston By Label")+ 
  facet_wrap(~ Label)

plot(g_7) 


g_8<- ggplot(data=train_factor,aes(x=DER_deltar_tau_lep,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_density(alpha = 0.3)+
  ggtitle("Distribution of Higgs_Boston By Label")+ 
  facet_wrap(~ Label)

plot(g_8)



# Scatterplot 
g_s8<- ggplot(data=train_factor,aes(x=PRI_met_sumet,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_point(aes(y=DER_pt_tot),alpha = 0.3)

plot(g_s8)


g_s9<- ggplot(data=train_factor,aes(x=DER_pt_h ,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_point(aes(y=DER_pt_tot),alpha = 0.3)+facet_wrap(~ Label)

plot(g_s9)

# Boxplot


g_b8<- ggplot(data=train_factor,aes(PRI_jet_num,PRI_met_sumet,fill=PRI_jet_num))+
  geom_boxplot(alpha=0.6)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=1.5)+
  ggtitle("Boxplot of Higgs_Boston By Jet Num")
  

ggplotly(g_b8)


g_b9<- ggplot(data=train_factor,aes(PRI_jet_num,DER_pt_tot,fill=PRI_jet_num))+
  geom_boxplot(alpha=0.6)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=1.5)+
  ggtitle("Boxplot of Higgs_Boston By Jet Num")
  

ggplotly(g_b9)

g_b10<- ggplot(data=train_factor,aes(PRI_jet_num,DER_pt_h,fill=PRI_jet_num))+
  geom_boxplot(alpha=0.6)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=1.5)+
  ggtitle("Boxplot of Higgs_Boston By Jet Num")


ggplotly(g_b10)


# #DER_mass_MMC Max.   :1192.03
# DER_mass_transverse_met_lep 444.719
# DER_pt_h   2834.999
# DER_pt_tot 2834.999  
# #DER_sum_pt  
# #DER_mass_vis   Max.   :1349.351
# DER_met_phi_centrality Min.   :-1.4140
# PRI_tau_phi  -2.981
# PRI_met   :2842.617 
#PRI_lep_pt 
#PRI_met_sumet :2003.98 
#PRI_jet_leading_pt 1120.57
#PRI_jet_subleading_pt 721.46  
# PRI_jet_all_pt  Max.   :1633.43  



################################################################
# Impute missing value by Principal Component Analysis -
################################################################
require(missMDA)
# # # estimate how many components are using for imputing 
nb_jet0 <- estim_ncpPCA(train_jet0,ncp.min = 0,ncp.max = 18)   
nb_jet0

nb_jet1 <- estim_ncpPCA(train_jet1,ncp.min = 0,ncp.max = 22)  #method.cv = "Kfold") ncp=17
nb_jet1  

nb_jet2.3 <- estim_ncpPCA(train_jet2.3,ncp.min = 0,ncp.max = 29)
nb_jet2.3

## impute the missing value
# pca_imputed<- imputePCA(train, ncp = 3, scale = TRUE, method = c("Regularized","EM"),
#           row.w = NULL, coeff.ridge = 1, threshold = 1e-06, seed = NULL, nb.init = 1,
#           maxiter = 1000)

pca_imputed_jet0<- imputePCA(train_jet0, ncp = 4, scale = TRUE, method = c("Regularized","EM"),
                             row.w = NULL, coeff.ridge = 1, threshold = 1e-06, seed = NULL, nb.init = 1,
                             maxiter = 1000)

pca_imputed_jet1<- imputePCA(train_jet1, ncp = 3, scale = TRUE, method = c("Regularized","EM"),
                        row.w = NULL, coeff.ridge = 1, threshold = 1e-06, seed = NULL, nb.init = 1,
                        maxiter = 1000)



pca_imputed_jet2.3<- imputePCA(train_jet2.3, ncp = 3, scale = TRUE, method = c("Regularized","EM"),
                             row.w = NULL, coeff.ridge = 1, threshold = 1e-06, seed = NULL, nb.init = 1,
                             maxiter = 1000)


cv_imputed_jet1<- imputePCA(train_jet1, ncp = 17, scale = TRUE, method = c("Regularized","EM"),
                             row.w = NULL, coeff.ridge = 1, threshold = 1e-06, seed = NULL, nb.init = 1,
                             maxiter = 1000)


train_jet0_final<- cbind.data.frame(pca_imputed_jet0$completeObs,train_jet0_track)
train_jet0_final <- select(train_jet0_final,EventId,everything())

train_jet1_final<- cbind.data.frame(pca_imputed_jet1$completeObs,train_jet1_track)
train_jet1_final <- select(train_jet1_final,EventId,everything())

train_jet2.3_final<- cbind.data.frame(pca_imputed_jet2.3$completeObs,train_jet2.3_track)
train_jet2.3_final <- select(train_jet2.3_final,EventId,everything())



###################################################################################################
write.csv(train_jet0_final,file="train_jet0_final.csv",row.names = F)
# write.csv(train_jet1_final,file="train_jet1_final.csv",row.names = F)
# write.csv(train_jet2.3_final,file="train_jet2.3_final.csv",row.names = F)

########################################
#Impute performance Compare by heat map#
########################################
train_jet_0_c <- train_jet0[complete.cases(train_jet0),] 
cor.jet_0<- round(cor(train_jet_0_c),2)
cor.imputed_jet_0<- round(cor(pca_imputed_jet0$completeObs),2)

train_jet_1_c <- train_jet1[complete.cases(train_jet1),] 
cor.jet_1<- round(cor(train_jet_1_c),2)
cor.imputed_jet_1<- round(cor(pca_imputed_jet1$completeObs),2)
cor.cv_imputed_jet_1 <- round(cor(cv_imputed_jet1$completeObs),2)


train_jet_2.3_c <- train_jet2.3[complete.cases(train_jet2.3),] 
cor.jet_2.3<- round(cor(train_jet_2.3_c),2)
cor.imputed_jet_2.3<- round(cor(pca_imputed_jet2.3$completeObs),2)




####Correlation Matrix of train_jet_0_complete Vs Correlation Matrix of imputed_jet_0
par(mfrow=c(1,2))
corrplot(cor.jet_0, type = "lower", #order="hclust", 
         tl.col="lightblue", tl.srt=50)


corrplot(cor.imputed_jet_0, type = "lower", #order="hclust", 
         tl.col="lightblue", tl.srt=50)

####Correlation Matrix of train_jet_1_complete Vs Correlation Matrix of imputed_jet_1
corrplot(cor.jet_1, type = "lower", #order="hclust", 
         tl.col="lightblue", tl.srt=50)

corrplot(cor.imputed_jet_1, type = "lower", #order="hclust", 
         tl.col="lightblue", tl.srt=50)

corrplot(cor.cv_imputed_jet_1, type = "lower", #order="hclust", 
         tl.col="lightblue", tl.srt=50)


####Correlation Matrix of train_jet_2.3_complete Vs Correlation Matrix of imputed_jet_2.3
corrplot(cor.jet_2.3, type = "lower", #order="hclust", 
         tl.col="lightblue", tl.srt=50)

corrplot(cor.imputed_jet_2.3, type = "lower", #order="hclust", 
         tl.col="lightblue", tl.srt=50)



#######################################
# ## PCA performance                 ##
#######################################
train_jet0_final <- read.csv("train_jet0_final.csv",header = T)
train_jet_final_feaOnly <- select(train_jet0_final,-EventId,-Weight,-Label)

pca_jet_0 <- PCA(train_jet_final_feaOnly, scale.unit = TRUE, ncp = 5, ind.sup = NULL,row.w = NULL,
     col.w = NULL, graph = F, axes = c(1,2))
print(pca_jet_0)
eig_jet_0 <- pca_jet_0$eig
head(eig_jet_0[, 1:2])
library("factoextra")
fviz_screeplot(pca_jet_0, ncp=18)

head(pca_jet_0$var$coord)

fviz_pca_var(pca_jet_0, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()
# The cos2 values are used to estimate the quality of the representation
# The closer a variable is to the circle of correlations, the better its representation on the factor map (and the more important it is to interpret these components)
# Variables that are closed to the center of the plot are less important for the first components.

head(pca_jet_0$var$contrib)

# Contributions of variables on PC1
fviz_contrib(pca_jet_0, choice = "var", axes = 1)
# If the contribution of the variables were uniform, the expected value would be 1/length(variables) = 1/18 = 5.56%.
# The red dashed line on the graph above indicates the expected average contribution. 
# For a given component, a variable with a contribution larger than this cutoff could be 
# considered as important in contributing to the component.


fviz_pca_contrib(pca_jet_0, choice = "var", axes = 1, top = 10)

# Control variable colors using their contributions
fviz_pca_var(pca_jet_0, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()


#Dimension description

pca_jet0.desc <- dimdesc(pca_jet_0, axes = 1:5)
# Description of dimension 1
pca_jet0.desc$Dim.1

pca_jet0.desc$Dim.2

pca_jet0.desc$Dim.3



#dimdesc(pca_jet_0, axes = 1:5, proba = 0.05)
# pca_jet_0 : an object of class PCA
# axes : a numeric vector specifying the dimensions to be described
# prob : the significance level
summary.PCA(pca_jet_0)

