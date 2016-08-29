# Preprocessing data
library(VIM)
library(mice)
library(adabag)
library(corrplot)
library(caret)
library(doMC)
library(neuralnet)
library(nnet)
library(devtools)
library(pROC)
library(clusterGeneration)
library(NeuralNetTools)
library(mice)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(psych)
library(ggpubr)

registerDoMC(cores = 4)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##------------------------------------------------------------------------------
# EDA: plot missingness and correlation
aggr(train_0)
aggr(train_1)
aggr(train_2)

##------------------------------------------------------------------------------
## Analyze imputated data
setwd("/Users/binfang/Documents/NYCDSA/project/Project_4/data")
subset0 <- read.csv('subset0.csv', header=T)
#subset0 <- subset0[, -c(1, 20, 21, 22, 24)]
subset1 <- read.csv('subset1.csv', header=T)
#subset1 <- subset1[, -c(1, 20, 25, 27)]
subset23 <- read.csv('subset23.csv', header=T)
#subset23 <- subset23[, -c(1, 24, 32, 34)]


# Correlation Plot
corrplot(cor(subset0), method="circle", type = "upper", order="hclust",
         col=colorRampPalette(brewer.pal(11,"Spectral"))(8))

corrplot(cor(subset1), method="circle", type = "upper", order="hclust",
         col=colorRampPalette(brewer.pal(11,"Spectral"))(8))

corrplot(cor(subset23), method="circle", type = "upper", order="hclust",
         col=colorRampPalette(brewer.pal(11,"Spectral"))(8))

# PCA
fa.parallel(subset23, #The data in question.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.
pc_bodies = principal(subset0, #The data in question.
                      nfactors = 9, #The number of PCs to extract.
                      rotate = "none")
pc_bodies

# Density Plot
# Use custom palette
p1 <- ggdensity(subset0, x = "DER_pt_h",
                add = "mean", rug = F,
                color = "Label", fill = "Label",
                palette = c("#00AFBB", "#E7B800")) + 
  labs(title = "PRI_jet_num=0")
p2 <- ggdensity(subset1, x = "DER_pt_h",
                add = "mean", rug = F,
                color = "Label", fill = "Label",
                palette = c("#00AFBB", "#E7B800")) +
  labs(title = "PRI_jet_num=1")
p3 <- ggdensity(subset23, x = "DER_pt_h",
                add = "mean", rug = F,
                color = "Label", fill = "Label",
                palette = c("#00AFBB", "#E7B800")) +
  labs(title = "PRI_jet_num=2&3")
multiplot(p1, p2, p3, cols=3)
