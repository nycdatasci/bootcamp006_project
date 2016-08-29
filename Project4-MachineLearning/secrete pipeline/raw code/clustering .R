#clustering

[10:34]  
get_df <- function (train,jet_num){
  #jet_num <- 0
  set <- subset(train, train$PRI_jet_num == jet_num)
  if (jet_num <= 1){
    features_to_keep <- c(colnames(set)[colSums(is.na(set)) > 0])[-1]
    set <- set[ , -which(names(set) %in% features_to_keep)]
  }  
  # order matters
  set$DER_mass_MMC[!is.na(set$DER_mass_MMC)] <- 1
  set[is.na(set)] <- 0
  
  set_cl <- kmeans(set[,2:ncol(set)], centers = 2, nstart = 3, iter.max = 10)
  set_label <- as.data.frame(set_cl$cluster)
  
  set_out <- cbind(set,set_label)
  return(set_out)
  
  result_table<- function(set){
    set <- table(set$DER_mass_MMC,set$`set_cl$cluster`)
    return(set)
  }