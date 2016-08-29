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

#data = data[, -c(1,32,33)]          # Get rid of EventId Weight Label
data$Nothingness = 0 

k = 1
for (i in 1:length(names(data))) {
    
    if(sum(rowSums(is.na(data)) ==i) !=0){
      data$Nothingness[rowSums(is.na(data)) ==i] = k
      k = k+1
    }
 }

data[is.na(data)] = -999.0
data[is.na(data)] = -999.0

write.csv(data, file = "training_nothingness.csv",row.names=FALSE)


