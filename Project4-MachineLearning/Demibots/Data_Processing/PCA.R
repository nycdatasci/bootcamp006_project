library(psych)
library(HSAUR)
library(dplyr)
library(VIM)
library(mice)
library(PASWR)
library(ggplot2)
library(MASS)
library(tree)
library(ISLR)
library(randomForest)
library(corrplot)
library(car)


setwd('/Users/dk1306/nycdsa-kaggle-project/Data_Processing/')
data = read.csv('training_nothingness.csv', header=T)

Label   = data$Label

#data = data[complete.cases(data),]
data = data[, -c(1,32,33)]
data = as.data.frame(scale(data))
data$Label = Label 
 

########################################################################

png(filename="plots/scree_plot.png")
fa.parallel(data[,-ncol(data)],n.obs = nrow(data),fa = "pc",n.iter = 300)
abline(h = 1)
dev.off()


########################################################################


png(filename="plots/PCA_percentage_plot.png")
nfactors = ncol(data) - 1

pc_data = principal(data[,-ncol(data)],nfactors = nfactors,rotate = "none")


y = pc_data$values[1:nfactors]/sum(pc_data$values[1:nfactors])*100
x = 1:nfactors

z =rep(0,nfactors)

for (i in 1:nfactors){
  z[i] = sum(y[1:i])
}

plot(x,z,
     xlab = "Number of Principal Components",
     ylab = "Percentage of variance explained",
     main = "PCA", las=1)
xticks = c(max(x[z <= 90]),max(x[z <= 95]), max(x[z <= 99]))
xticks = sort(xticks)
yticks = c(95)
axis(side = 1, at = xticks)
axis(side = 2, at = yticks, las=1)
abline(h = 90,v =  max(x[z <= 90]),col = 'red', lty = 4)
abline(h = 95,v =  max(x[z <= 95]),col = 'blue', lty = 6)
abline(h = 99,v =  max(x[z <= 99]),col = 'green', lty = 8)
dev.off()

########################################################################

colnames(data[,-ncol(data)]) = 1:ncol(data[,-ncol(data)])

for(i in 1:max(x[z <= 90])){
  nfactors = i
  png(filename=paste0(i,"_factor_plot.png"))
  pc_data = principal(data[,-ncol(data)],nfactors = nfactors,rotate = "none")
  factor.plot(pc_data,labels = colnames(data[,-ncol(data)]))
  dev.off()
}


#################################################


df = as.data.frame(cbind(pc_data$scores,Label))
df$Label = as.factor(Label)

plots <- list() 

for (nm in colnames(df)[-ncol(df)]){
  plots[[nm]] <- ggplot(data = df, aes_string(x=nm)) +
    geom_density(aes(color = Label))+
    geom_density()
  }

for (nm in names(plots)){
  print(plots[nm])
  ggsave(paste0(nm,"_density.png"))
}

#################################################

data_corr = df[-ncol(df)]    
M = round(cor(data_corr),2)                # Correlation matrix rounded to two significant digits 




type="full"         # "full" , "upper" , "lower"
method="circle"     # "circle", "square", "ellipse", "number", "shade", "color", "pie"
order = "hclust"        # "AOE", "FPC", "hclust", "alphabet"




png(filename="plots/PC_correlation.png",  
    width = 800 ,
    height = 600)
corrplot(M, type=type,         #Full correlation
         method=method,
         order =order,
         cl.ratio=0.2,
         cl.colalign="r" )
dev.off()

