library(ggplot2)
library(reshape2)
setwd("C:/Users/ricky/dropbox/bootcamp/capstone/")
bp.df = read.csv("boruta_final.csv")
bp.df = bp.df[,-1]

bp = melt(bp.df)
bp$variable <- with(bp, reorder(variable, value, median))

impPlot = ggplot(bp, aes(variable, value)) + 
          geom_boxplot() +
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
          labs(title = "Feature Importance Plot from Random Forest(Boruta)", 
               x = "Feature", y = "Importance")
         
impPlot
          

