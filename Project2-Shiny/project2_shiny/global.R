myimage<-NULL
searchframe<-readRDS('facefeature.rds')
dataframe_list<-readRDS('dataframe_list.rds')
charlen<-36

wholelist<-c("faceRectangle.height","faceAttributes.smile","faceAttributes.headPose.roll","faceAttributes.headPose.yaw","faceAttributes.gender","faceAttributes.age")
wholelist<-c(wholelist,"faceAttributes.facialHair.moustache","faceAttributes.facialHair.beard","faceAttributes.facialHair.sideburns","scores.anger","scores.contempt","scores.disgust")
wholelist<-c(wholelist,"scores.fear","scores.happiness","scores.neutral","scores.sadness","scores.surprise","faceRectangle.width")

discretelist<-c('faceAttributes.gender','faceAttributes.facialHair.moustache','faceAttributes.facialHair.beard','faceAttributes.facialHair.sideburns')
continuelist<-wholelist [! wholelist %in% discretelist]



facekey = '8fd43d9ea23045ed86f365eed9500ffc'
emotionkey = '355b73f0ac794563adf7898511373448'

path=NULL