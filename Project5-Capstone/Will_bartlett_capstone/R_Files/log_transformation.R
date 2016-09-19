setwd('~/Desktop/Data_Science/GCS_Folder/Rdata_Files')
load('model_data_min.RData')

transform = function(score, hour){
      (score-12)*(log(hour))
}

model_data_min$log_12 = transform(model_data_min$score, model_data_min$hour)
save(model_data_min, file = "model_data_min.RData")
