setwd("~/Desktop/Kaggle_Jumpstart")
library(car)
library(psych)
library(FactoMineR)
library(plotly)
library("factoextra")
library(corrplot)
library(dplyr)
library(ggplot2)


# Load Imputed Clean Data
train_jet0_C <- read.csv("Final_data/train_jet_0",header = T)
train_jet1_C <- read.csv("Final_data/train_jet_1",header = T)
train_jet2.3_C <- read.csv("Final_data/train_jet_2_3",header = T)

test_jet0_C <-read.csv("Final_data/test_jet_0",header = T)
test_jet1_C <- read.csv("Final_data/test_jet_1",header = T) 
test_jet2.3_C <- read.csv("Final_data/test_jet_2_3",header = T) 

#Keep the feature variable Only  
train_jet0_feaOnly <- select(train_jet0_C,-EventId,-Weight,-Label)
train_jet1_feaOnly <- select(train_jet1_C,-EventId,-Weight,-Label)
train_jet2.3_feaOnly <- select(train_jet2.3_C,-EventId,-Weight,-Label)

#


# ########################################
# #Check Impute performance              #
# ########################################
train_jet_0_c <- train_jet0[complete.cases(train_jet0),]
cor.jet_0<- round(cor(train_jet0,use = "everything"),2)
cor.train_jet0_C<- round(cor(train_jet0_C[,-c(1,20,21)]),2)

train_jet_1_c <- train_jet1[complete.cases(train_jet1),]
cor.jet_1<- round(cor(train_jet_1_c),2)
cor.train_jet1_C<- round(cor(train_jet1_C[,-c(1,24,25)]),2)

train_jet_2.3_c <- train_jet2.3[complete.cases(train_jet2.3),]
cor.jet_2.3<- round(cor(train_jet_2.3_c),2)
cor.train_jet2.3_C<- round(cor(train_jet2.3_C[,-c(1,31,32)]),2)


###Correlation Matrix of train_jet_0_complete Vs Correlation Matrix of imputed_jet_0
par(mfrow=c(1,2))
corrplot(cor.jet_0, type = "lower", #order="hclust",
         tl.col="lightblue", tl.srt=50)


corrplot(cor.imputed_jet_0_S, type = "lower", #order="hclust",
         tl.col="lightblue", tl.srt=50)

####Correlation Matrix of train_jet_1_complete Vs Correlation Matrix of imputed_jet_1
corrplot(cor.jet_1, type = "lower", #order="hclust",
         tl.col="lightblue", tl.srt=50)

corrplot(cor.train_jet1_C, type = "lower", #order="hclust",
         tl.col="lightblue", tl.srt=50)


####Correlation Matrix of train_jet_2.3_complete Vs Correlation Matrix of imputed_jet_2.3
corrplot(cor.jet_2.3, type = "lower", #order="hclust",
         tl.col="lightblue", tl.srt=50)

corrplot(cor.train_jet2.3_C, type = "lower", #order="hclust",
         tl.col="lightblue", tl.srt=50)

########################################
par(mfrow=c(1,1))
cor.train_jet0<- round(cor(train_jet0_feaOnly),2)
corrplot(cor.train_jet0, type = "lower", #order="hclust",
         tl.col="lightblue", tl.srt=50)

cor.train_jet1<- round(cor(train_jet1_feaOnly),2)
corrplot(cor.train_jet1, type = "lower", #order="hclust", 
         tl.col="lightblue", tl.srt=50)#mar = c(5,0,0,0),


cor.train_jet2.3<- round(cor(train_jet2.3_feaOnly),2)
corrplot(cor.train_jet2.3, type = "lower", #order="hclust", 
         tl.col="lightblue", tl.srt=50)





#Visualize correlation matrix using a heatmap +hierarchical structure

col<- colorRampPalette(c("red","white", "blue"))(20)
# par(mfrow=c(1,1))
heatmap(x =cor.train_jet0, col = col, symm = TRUE)
heatmap(x =cor.train_jet1, col = col, symm = T)
heatmap(x =cor.train_jet2.3, col = col, symm = TRUE)





########################################
# PCA Feature Selection                #
########################################



pca_jet_0 <- PCA(train_jet0_feaOnly, scale.unit = TRUE, ncp = 10, ind.sup = NULL,row.w = NULL,
                 col.w = NULL, graph = F, axes = c(1,2))
print(pca_jet_0)
eig_jet_0 <- pca_jet_0$eig
head(eig_jet_0[, 1:2])

pca_jet_1 <- PCA(train_jet1_feaOnly, scale.unit = TRUE, ncp = 15, ind.sup = NULL,row.w = NULL,
                 col.w = NULL, graph = F, axes = c(1,2))


pca_jet_2.3 <- PCA(train_jet2.3_feaOnly, scale.unit = TRUE, ncp = 25, ind.sup = NULL,row.w = NULL,
                 col.w = NULL, graph = F, axes = c(1,2))


library("factoextra")
fviz_screeplot(pca_jet_0, ncp=18)
fviz_screeplot(pca_jet_1, ncp=22)
fviz_screeplot(pca_jet_2.3, ncp=29)


fviz_pca_var(pca_jet_0, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()

fviz_pca_var(pca_jet_1, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()

fviz_pca_var(pca_jet_2.3, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()


# The cos2 values are used to estimate the quality of the representation
# The closer a variable is to the circle of correlations, the better its representation on the factor map (and the more important it is to interpret these components)
# Variables that are closed to the center of the plot are less important for the first components.

head(pca_jet_0$var$contrib)

# Contributions of variables on PC1
fviz_contrib(pca_jet_0, choice = "var", axes = 1)
fviz_contrib(pca_jet_1, choice = "var", axes = 1)
# If the contribution of the variables were uniform, the expected value would be 1/length(variables) = 1/18 = 5.56%.
# The red dashed line on the graph above indicates the expected average contribution. 
# For a given component, a variable with a contribution larger than this cutoff could be 
# considered as important in contributing to the component.


fviz_contrib(pca_jet_0, choice = "var", axes = 2, top = 10)

# Control variable colors using their contributions
fviz_pca_var(pca_jet_0, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()

fviz_pca_var(pca_jet_1, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()


fviz_pca_var(pca_jet_2.3, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()


#Dimension description

pca_jet0.desc <- dimdesc(pca_jet_0, axes = 1:10)
pca_jet1.desc <- dimdesc(pca_jet_1, axes = 1:4)


# Description of dimension 1
pca_jet0.desc$Dim.1
pca_jet1.desc$Dim.1
pca_jet0.desc$Dim.2

pca_jet0.desc$Dim.3

pca_jet0.desc$Dim.4

pca_jet0.desc$Dim.5

pca_jet0.desc$Dim.6

pca_jet0.desc$Dim.7

#dimdesc(pca_jet_0, axes = 1:5, proba = 0.05)
# pca_jet_0 : an object of class PCA
# axes : a numeric vector specifying the dimensions to be described
# prob : the significance level
summary.PCA(pca_jet_0)


###########################################
# Visulization  histogram by label       #
###########################################
train_jet0_C$DER_mass_MMC_square <- train_jet0_C$DER_mass_MMC**2

plotDensityFun <- function(x, na.rm = TRUE, ...) {
  nm <- names(x)
  label <- x[,ncol(x)]
  for (i in seq_along(nm)) {
    print(ggplot(x,aes_string(x = nm[i])) + 
            geom_density(aes(fill=label,colour=label),alpha = .3))
    #ggsave(plots,filename=paste("myplot",nm[i],".png",sep=""),width = 800, height = 788)
    }
}

plotDensityFun(train_jet0_C)
plotDensityFun(train_jet1_C)
plotDensityFun(train_jet2.3_C)

plotDensityFun(phi)

d_1 <- ggplot(train_jet0_C,aes(x=PRI_tau_phi)) + 
  geom_density(aes(fill=Label,colour=Label),alpha = .3)
plot(d_1)

##  phi comprison by label  
d_2 <- ggplot(train_factor,aes(x=PRI_tau_phi)) + 
  geom_density(aes(fill=Label,colour=Label),alpha = .3)+
  facet_wrap(~ PRI_jet_num)
plot(d_2)

### DER_pt_h & DER_pt_tot
d_3 <- ggplot(train_jet0_C,aes(x=DER_pt_h)) + 
  geom_density(alpha = .3)
plot(d_3)



#plotly_POST(d_2, filename = "/PRI_tau_phi", fileopt = "overwrite",sharing = "public")

# boxplot
b_1<- ggplot(data=train_jet0_C,aes(Label,DER_mass_transverse_met_lep,fill=Label))+
  geom_boxplot(alpha=0.6)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=1.5)+
  ggtitle("Boxplot of Higgs_Boston By Jet Num")

ggplotly(b_1)



b_2<- ggplot(data=train_factor,aes(Label,PRI_tau_phi,fill=Label))+
  geom_boxplot(alpha=0.6)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=1.5)+
  facet_wrap(~ PRI_jet_num)+
  ggtitle("Boxplot of PRI_tau_phi by Jet Num")+
  xlab("PRI_jet_num")

ggplotly(b_2)


b_2<- ggplot(data=train_factor,aes(Label,DER_pt_h,fill=Label))+
  geom_boxplot(alpha=0.6)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=1.5)+
  facet_wrap(~ PRI_jet_num)+
  ggtitle("Boxplot of PRI_tau_phi by Jet Num")+
  xlab("PRI_jet_num")

ggplotly(b_2)




# scatter plot 
g_s9<- ggplot(data=train_jet0_C,aes(x=DER_pt_h))+
  geom_point(aes(y=DER_pt_tot),alpha = 0.3)

ggplotly(g_s9)

g_s9<- ggplot(data=train_factor,aes(x=DER_pt_h ,fill=PRI_jet_num,colour=PRI_jet_num))+
  geom_point(aes(y=DER_pt_tot),alpha = 0.3)+facet_wrap(~ Label)

plot(g_s9)


# plotly_POST publishes the figure to your plotly account on the web
plotly_POST(b_2, filename = "/Blog", fileopt = "overwrite",sharing = "public")





