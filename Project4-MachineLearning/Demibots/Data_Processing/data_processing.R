library(dplyr)
library(VIM)
library(mice)
library(PASWR)
library(ggplot2)
library(MASS)
library(corrplot)
library(car)

setwd('/Users/dk1306/nycdsa-kaggle-project/Data_Processing/')

path_to_data_file = '/Users/dk1306/downloads/training.csv'
data = read.csv(path_to_data_file, header=T)

data[data==-999.0] = NA
data[data==-999.0] = NA

EventId = data$EventId
Weight  = data$Weight
Label   = data$Label

data = data[, -c(1,32,33)]          # Get rid of EventId Weight Label
#PRI_jet_num = data$PRI_jet_num     # Jet number is a category variable so do not scale it
data = as.data.frame(scale(data))   # Scale data, it is scaling Jet number also 
#data$PRI_jet_num = PRI_jet_num     # Use unscaled Jet number in the data

tf = colSums(is.na(data)) !=0       # Columns missing any data 

length(colnames(data)[tf])          # Number of columns missing any data
#[1] 11

ph = as.data.frame(colSums(is.na(data))[colnames(data)[colSums(is.na(data)) !=0] ]/nrow(data)*100 )    # Column Names and % of missing data 
colnames(ph) = "per_undef"
ph
#per_undef
#DER_mass_MMC             15.2456
#DER_deltaeta_jet_jet     70.9828
#DER_mass_jet_jet         70.9828
#DER_prodeta_jet_jet      70.9828
#DER_lep_eta_centrality   70.9828
#PRI_jet_leading_pt       39.9652
#PRI_jet_leading_eta      39.9652
#PRI_jet_leading_phi      39.9652
#PRI_jet_subleading_pt    70.9828
#PRI_jet_subleading_eta   70.9828
#PRI_jet_subleading_phi   70.9828
table(ph)
#ph
#15.2456 39.9652 70.9828 
#     1       3       7 
sum(data$PRI_jet_num == 0)/nrow(data)*100
#[1] 39.9652
sum((data$PRI_jet_num == 0)| (data$PRI_jet_num == 1))/nrow(data)*100
#[1] 70.9828
table(data[is.na(data$DER_mass_MMC),"PRI_jet_num"])/sum(table(data[is.na(data$DER_mass_MMC),"PRI_jet_num"]))*100
#        0         1         2         3 
#68.539119 19.840479  7.745185  3.875216 

data.variable = as.data.frame(cbind(data,"Label"= as.factor(Label)))
data.variable = data.variable[is.na(data.variable$DER_mass_MMC),][-1]


plots <- list()
for (nm in names(data.variable)[-31]){
  plots[[nm]] <- ggplot(data = data.variable, aes_string(x=nm)) + geom_density(aes(color = Label)) +  geom_density()
  
}

for (nm in names(plots)){
  print(plots[nm])
  ggsave(paste0(nm,"undefinedhiggs_mass_density.png"))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Histograms

plots <- list()
for (nm in names(data.variable)[-31]){
  plots[[nm]] <- ggplot(data = data.variable, aes_string(x=nm)) + geom_histogram(aes(fill = Label),bins = 500,position = "dodge") 
  
}

for (nm in names(plots)){
  print(plots[nm])
  ggsave(paste0(nm,"_undefinedhiggs_mass_density_histogram.png"))
}




colnames(data)[which(colSums(is.na(data))[colnames(data)[colSums(is.na(data)) !=0] ]/nrow(data)*100 >70)] # Name of Columns missing more than 70 % data
#[1] "DER_mass_transverse_met_lep" "DER_mass_vis"                "DER_pt_h"                    "DER_deltaeta_jet_jet"       
#[5] "DER_pt_tot"                  "DER_sum_pt"                  "DER_pt_ratio_lep_tau" 

sum(is.na(data))/ (nrow(data)* ncol(data))*100    # % of cells missing data
#[1] 21.06736

sum(complete.cases(data)) / nrow(data)*100    # % of events/rows with complete data
#[1] 27.2456

aggr(data)                   # Display Missinggness
md.pattern(data)             # Missingness in numerical format 

#####################################################################################

Response = as.data.frame(cbind("Label" = as.factor(Label),Weight))       # EDA on predictions  
Response$Label = Label

#~~~~~~~~~~~~~

n.bin = 20                                         # Number of categories to split Weight
labels=c(1:n.bin)                                  # Labeling the new categories 
f = cut(Response$Weight,n.bin ,labels=labels)      # Categorical variable for scaled weights
tbl = table(Label,f)
tbl
#Label     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20
#b 23389 10550  5629 11176 23949 24594 12135  5834  1343   358  3278 11736 12959  6762  4235  2780  2023  1138   413    52
#s 85667     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0

chisq.test(tbl)                       # Weights and Label are dependent on each other 
#Pearson's Chi-squared test
#data:  tbl
#X-squared = 168430, df = 19, p-value < 2.2e-16

#~~~~~~~~~~~~~

max((Response$Weight[Response$Label == 's'])) < min(Response$Weight[Response$Label == 'b']) # Signal and background are mutually exclusive in weights
#[1] TRUE

#Signal events have low weights 


#~~~~~~~~~~~~~
g = ggplot(data = Response, aes(x =Weight))
g + geom_histogram(aes(fill = Label), binwidth = 0.1)
ggsave("plots/s_AND_b_weight_histogram.png")


ggplot(data = as.data.frame(Response), aes(x =Weight))  +
  geom_density()
ggsave("plots/s_AND_b_weight_density.png")

g = ggplot(data = Response[Response$Weight <= max(Response$Weight[Response$Label == 's']),], aes(x =Weight))
g + geom_histogram(aes(fill = Label), binwidth = 0.0002)
ggsave("plots/s_weight_histogram.png")

ggplot(data = as.data.frame(Response[Response$Weight <= max(Response$Weight[Response$Label == 's']),]), aes(x =Weight))  +
  geom_density()
ggsave("plots/s_weight_density.png")

g = ggplot(data = Response[Response$Weight > max(Response$Weight[Response$Label == 's']),], aes(x =Weight))
g + geom_histogram(aes(fill = Label), binwidth = 0.1)
ggsave("plots/b_weight_histogram.png")

ggplot(data = Response[Response$Weight > max(Response$Weight[Response$Label == 's']),], aes(x =Weight))  +
  geom_density()
ggsave("plots/b_weight_density.png")

#####################################################################################

data.variable = as.data.frame(cbind(data,"Label" = as.factor(Label)))    
data.variable$Label = Label

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Density 
plots <- list()
for (nm in names(data.variable)[-31]){
  plots[[nm]] <- ggplot(data = data.variable, aes_string(x=nm)) + geom_density(aes(color = Label)) +  geom_density()
                                                                                                 
}

for (nm in names(plots)){
  print(plots[nm])
  ggsave(paste0(nm,"_density.png"))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Histograms

plots <- list()
for (nm in names(data.variable)[-31]){
  plots[[nm]] <- ggplot(data = data.variable, aes_string(x=nm)) + geom_histogram(aes(fill = Label),bins = 500,position = "dodge") 
  
}

for (nm in names(plots)){
  print(plots[nm])
  ggsave(paste0(nm,"_histogram.png"))
}

########################################################################

data_corr = data[complete.cases(data),]    # Events with complete data 
M = round(cor(data_corr),2)                # Correlation matrix rounded to two significant digits 


colnames(M) = c(1:nrow(M))                 # Renaming columns for ease of viewing plot 
rownames(M) =  paste(c(1:nrow(M)), rownames(M), sep = ') ')   # Tagging row names with column indices of M 


type="full"         # "full" , "upper" , "lower"
method="circle"     # "circle", "square", "ellipse", "number", "shade", "color", "pie"
order = "hclust"        # "AOE", "FPC", "hclust", "alphabet"
addrect=3



png(filename="plots/full_correlation.png",  
    width = 800 ,
    height = 600)
corrplot(M, type=type,         #Full correlation
         method=method,
         order =order,
         addrect=addrect,
         cl.ratio=0.2,
         cl.align="r" )
dev.off()


#Test of siginificance of correlation 

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(data_corr,0.95)         #95 % significance
res2 <- cor.mtest(data_corr,0.99)         #95 % significance



type="lower" 

png(filename="plots/siginficant_correlation.png",width = 1000 ,height = 600)
corrplot(M, type=type,            #crossing out less significant correlations
         method=method,
         order =order,
         addrect=addrect, 
         p.mat = res1[[1]], 
         sig.level=0.05,
         insig = "pch")
dev.off()

png(filename="plots/confident_interval_siginficant_correlation.png",
    width = 1000 ,
    height = 600)
corrplot(M, p.mat = res1[[1]],                   #Confidence Interval
         low=res1[[2]], upp=res1[[3]],            
         method=method,
         order =order,
         pch.col="red",
         sig.level = 0.05,
         addrect=3,
         rect.col="navy",
         plotC="rect",
         cl.pos="n", 
         type=type)
dev.off()
