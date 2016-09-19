library(psych)

setwd("~/Desktop/Data_Science/GCS_Folder/Rdata_Files")

load('makeup.RData')

makeup_pca = makeup[,c(3,4,5)]

fa.parallel(makeup_pca, 
          fa = 'pc',
          n.iter = 100)
pc_makeup = principal(makeup_pca,
          nfactors = 2,
          rotate = 'none')
pc_makeup


factor.plot(pc_makeup,
            labels = colnames(makeup_pca))

