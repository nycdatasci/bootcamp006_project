
install.packages("doMC")
install.packages("png")
install.packages("grid")
install.packages('reshape2')
install.packages('foreach')

library(png)
library(grid)
library(doMC)
library(dplyr)
library(ggplot2)
library(reshape2)
library(foreach)

registerDoMC()

training=read.csv("training.csv",stringsAsFactors=F)
image_train=training$Image
training$Image=NULL

image_train <- foreach(im = image_train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

testing=read.csv("test.csv",stringsAsFactors=F)
image_test=testing$Image
testing$Image=NULL

image_test <- foreach(im = image_test, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

save(training,image_train,testing,image_test,file='data.Rd')

#Basic Analysis

#Show the Image
image_sample=matrix(data=rev(image_train[1,]),nrow=96,ncol=96)
image(1:96,1:96,image_sample,col=gray((0:255)/255))

#Plot the central points as important features to identify a person


temp_frame=data.frame(xaxis=rep(0,length(select(training[1,],ends_with("_x")))))

temp_frame['xaxis']=as.numeric(select(training[1,],ends_with("_x")))
temp_frame$yaxis=as.numeric(select(training[1,],ends_with("_y")))

points(96-temp_frame$xaxis,96-temp_frame$yaxis,col="red")

#Plot all the central points to see the overall distribution 

all_frame=data.frame(xaxis=rep(0,ncol(select(training,ends_with("_x")))*nrow(select(training,ends_with("_x")))))
all_frame$xaxis=96-as.vector(t(select(training,ends_with("_x"))))
all_frame$yaxis=96-as.vector(t(select(training,ends_with("_y"))))

ggplot(all_frame,aes(x=xaxis,y=yaxis))+geom_point()

#color the points for different parts

point_class=c(rep('eye',6),rep('eyebrow',4),rep('nose',1),rep('mouth',4))
point_class=rep(point_class,nrow(training))
all_frame$class=point_class

ggplot(all_frame,aes(x=xaxis,y=yaxis))+geom_point(aes(color = point_class))

#Add the average point for each part

detail_class=names(training)[seq(1, length(names(training)), 2)]
detail_class=substr(detail_class,1,nchar(detail_class)-2)
detail_class=rep(detail_class,nrow(training))
all_frame$detail_class=detail_class

avg_frame=summarise(group_by(all_frame,detail_class),avg_x=mean(xaxis,na.rm = TRUE),avg_y=mean(yaxis,na.rm = TRUE))

ggplot(data=avg_frame,aes(x=avg_x,y=avg_y))+geom_point()

#Let's see what happens to those extreme samples

index=which.max(training$left_eye_center_x)
im=matrix(data=rev(image_train[index,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-training[index,]$left_eye_center_x, 96-training[index,]$left_eye_center_y, col="red")

index=which.min(training$right_eye_center_y)
im=matrix(data=rev(image_train[index,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-training[index,]$right_eye_center_x, 96-training[index,]$right_eye_center_y, col="red")


#Simple distribution analysis
#Meaningless
#A face can appear at any location in a picture

ggplot(data=training,aes(x=left_eye_center_x))+geom_histogram(binwidth = 5)

ggplot(data=training,aes(x=right_eye_center_x))+geom_histogram(binwidth = 5)


#Instead of looking at a single feature, we could analyze the correlation between multiple 
#features. Like Euclidean distance between left eye centers and right eye centers

training$eye_dis=sqrt((training$left_eye_center_x-training$right_eye_center_x)^2+(training$left_eye_center_y-training$right_eye_center_y)^2)
ggplot(data=training,aes(x=eye_dis))+geom_histogram(binwidth = 1)

training$left_brow_len=sqrt((training$left_eyebrow_inner_end_x-training$left_eyebrow_outer_end_x)^2+(training$left_eyebrow_inner_end_y-training$left_eyebrow_outer_end_y)^2)
ggplot(data=training,aes(x=left_brow_len))+geom_histogram(binwidth = 0.5)

training$right_brow_len=sqrt((training$right_eyebrow_inner_end_x-training$right_eyebrow_outer_end_x)^2+(training$right_eyebrow_inner_end_y-training$right_eyebrow_outer_end_y)^2)
ggplot(data=training,aes(x=right_brow_len))+geom_histogram(binwidth = 0.5)

ggplot(data=training,aes(x=left_brow_len,y=right_brow_len))+geom_smooth()

#Lets make a simple try by assigning all the keypoints of test images the 
#same location, the mean value of all the keypoints in training images
#When submitting this online, you will get a very low score 3.96244
new_training=training[names(training)[1:30]]
mean_key= matrix(data=colMeans(new_training, na.rm=T), nrow=nrow(testing), ncol=ncol(new_training), byrow=T)
colnames(mean_key) = names(new_training)
predictions = data.frame(ImageId = 1:nrow(testing), mean_key)
submission = melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")

example.submission = read.csv('submissionFileFormat.csv')
sub.col.names      = names(example.submission)
example.submission$Location = NULL
submission = merge(example.submission, submission, all.x=T, sort=F)
submission = submission[, sub.col.names]
write.csv(submission, file="submission_means.csv", quote=F, row.names=F)


#Since the website doesn't give us the answer for the test set, we 
#can build a complete test set by our own through splitting the original
#training set into two sub sets training and testing.

#We will split the whole training set through four different ways:
#5 equivalent subsets, 10 equivalent subsets, 15 equivalent subsets, 20 equivalent subsets
#In each way, one out of the whole subsets would be used as the test set while the others 
#to be the training sets.


new_training$group_5=as.character(c(0:(nrow(new_training)-1))%%5)
  
new_training$group_10=as.character(c(0:(nrow(new_training)-1))%%10)
  
new_training$group_15=as.character(c(0:(nrow(new_training)-1))%%15)
  
new_training$group_20=as.character(c(0:(nrow(new_training)-1))%%20)



group5=summarise(group_by(new_training,group_5),count=n(),sum=sum(left_eye_center_x,na.rm = T))

result=c()
for (i in 1:nrow(group5))
{
  
  result=c(result,sum(group5[-i,'sum'])/sum(group5[-i,'count']))
  
}
group5$mean=result

ggplot()+geom_boxplot(data=new_training,aes(x=group_5,y=left_eye_center_x))+geom_point(data=group5,aes(x=group_5,y=mean,color='red'))

#Now lets try different splitting methods

group10=summarise(group_by(new_training,group_10),count=n(),sum=sum(left_eye_center_x,na.rm = T))

result=c()
for (i in 1:nrow(group10))
{
  
  result=c(result,sum(group10[-i,'sum'])/sum(group10[-i,'count']))
  
}
group10$mean=result

group15=summarise(group_by(new_training,group_15),count=n(),sum=sum(left_eye_center_x,na.rm = T))

result=c()
for (i in 1:nrow(group15))
{
  
  result=c(result,sum(group15[-i,'sum'])/sum(group15[-i,'count']))
  
}
group15$mean=result

group20=summarise(group_by(new_training,group_20),count=n(),sum=sum(left_eye_center_x,na.rm = T))

result=c()
for (i in 1:nrow(group20))
{
  
  result=c(result,sum(group20[-i,'sum'])/sum(group20[-i,'count']))
  
}
group20$mean=result

group=c(rep("05",5),rep("10",10),rep("15",15),rep("20",20))
result=data.frame(group)


result2=c()
for (i in 1:5)
{
  index=as.character(i-1)
  temp=(as.vector(select(filter(new_training,group_5==index),left_eye_center_x))-group5$mean[i])^2
  result2=c(result2,sqrt(mean(temp,na.rm = T)))
}

for (i in 1:10)
{
  index=as.character(i-1)
  temp=(as.vector(select(filter(new_training,group_10==index),left_eye_center_x))-group10$mean[i])^2
  result2=c(result2,sqrt(mean(temp,na.rm = T)))
}

for (i in 1:15)
{
  index=as.character(i-1)
  temp=(as.vector(select(filter(new_training,group_15==index),left_eye_center_x))-group15$mean[i])^2
  result2=c(result2,sqrt(mean(temp,na.rm = T)))
}

for (i in 1:20)
{
  index=as.character(i-1)
  temp=(as.vector(select(filter(new_training,group_20==index),left_eye_center_x))-group20$mean[i])^2
  result2=c(result2,sqrt(mean(temp,na.rm = T)))
}

result$rmse=result2

ggplot(data=result,aes(x=group,y=rmse,color=group))+geom_point()

#It seems different splitting methods don't influence much on the result.
#Now let's use a comparatively more complicated method called
#patching



patch_size = 10
coord_x = "left_eye_center_x"
coord_y = "left_eye_center_y"
patch_training=new_training[,1:30]

set.seed(0)
idxs     = sample(nrow(patch_training), nrow(patch_training)*0.8)

patch_test   = patch_training[-idxs, ]

patch_training  = patch_training[idxs, ]

patch_train_image <- image_train[idxs,]
patch_test_image  <- image_train[-idxs,]
rm("d", "im")

patches = foreach (i = 1:nrow(patch_training), .combine=rbind) %do% {
  im  = matrix(data = image_train[i,], nrow=96, ncol=96)
  x   = patch_training[i, coord_x]
  y   = patch_training[i, coord_y]
  x1  = (x-patch_size)
  x2  = (x+patch_size)
  y1  = (y-patch_size)
  y2  = (y+patch_size)
  if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
  {
    as.vector(im[x1:x2, y1:y2])
  }
  else
  {
    NULL
  }
}

mean_patch_10 = matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)


image(1:21, 1:21, mean_patch_10[21:1,21:1], col=gray((0:255)/255))


#Now lets test our models one by one

mean_patch=list(mean_patch_2,mean_patch_5,mean_patch_10)
search_size=c(2,5,10)


result_list=list()

  for (search_sample in search_size)
  {
    mean_x <- mean(patch_training[, coord_x], na.rm=T)
    mean_y <- mean(patch_training[, coord_y], na.rm=T)
    x1     <- as.integer(mean_x)-search_sample
    x2     <- as.integer(mean_x)+search_sample
    y1     <- as.integer(mean_y)-search_sample
    y2     <- as.integer(mean_y)+search_sample
    params <- expand.grid(x = x1:x2, y = y1:y2)
    for (patch_sample in mean_patch)
    {
      best_vector=c()

      for (i in 1:nrow(patch_test_image))
      {
       
        patch_size=(nrow(patch_sample)-1)/2
        im <- matrix(data = patch_test_image[i,], nrow=96, ncol=96)
        r  <- foreach(j = 1:nrow(params), .combine=rbind) %dopar% {
          x     <- params$x[j]
          y     <- params$y[j]
          p     <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
          score <- cor(as.vector(p), as.vector(patch_sample))
          score <- ifelse(is.na(score), 0, score)
          data.frame(x, y, score)
        }
        
        best <- r[which.max(r$score), c("x", "y")]
        best_vector=rbind(best_vector,best)
        
        
      }
      print(best_vector)
      print(patch_sample)
      result_list=c(result_list,list(best_vector))
    }
  }

test_id=c(1:nrow(patch_test_image))

test_result=data.frame(test_id)

test_result$real=patch_test$left_eye_center_x
test_result$real_y=patch_test$left_eye_center_y

test_result$two_two=result_list[[1]]$x
test_result$two_two_y=result_list[[1]]$y
test_result$two_five=result_list[[2]]$x
test_result$two_five_y=result_list[[2]]$y
test_result$two_ten=result_list[[3]]$x
test_result$two_ten_y=result_list[[3]]$y
test_result$five_two=result_list[[4]]$x
test_result$five_two_y=result_list[[4]]$y
test_result$five_five=result_list[[5]]$x
test_result$five_five_y=result_list[[5]]$y
test_result$five_ten=result_list[[6]]$x
test_result$five_ten_y=result_list[[6]]$y
test_result$ten_two=result_list[[7]]$x
test_result$ten_two_y=result_list[[7]]$y
test_result$ten_five=result_list[[8]]$x
test_result$ten_five_y=result_list[[8]]$y
test_result$ten_ten=result_list[[9]]$x
test_result$ten_ten_y=result_list[[9]]$y

test_result$two_two_euc=sqrt((test_result$real-test_result$two_two)^2+(test_result$real_y-test_result$two_two_y)^2)
test_result$two_five_euc=sqrt((test_result$real-test_result$two_five)^2+(test_result$real_y-test_result$two_five_y)^2)
test_result$two_ten_euc=sqrt((test_result$real-test_result$two_ten)^2+(test_result$real_y-test_result$two_ten_y)^2)


test_result$five_two_euc=sqrt((test_result$real-test_result$five_two)^2+(test_result$real_y-test_result$five_two_y)^2)
test_result$five_five_euc=sqrt((test_result$real-test_result$five_five)^2+(test_result$real_y-test_result$five_five_y)^2)
test_result$five_ten_euc=sqrt((test_result$real-test_result$five_ten)^2+(test_result$real_y-test_result$five_ten_y)^2)

test_result$ten_two_euc=sqrt((test_result$real-test_result$ten_two)^2+(test_result$real_y-test_result$ten_two_y)^2)
test_result$ten_five_euc=sqrt((test_result$real-test_result$ten_five)^2+(test_result$real_y-test_result$ten_five_y)^2)
test_result$ten_ten_euc=sqrt((test_result$real-test_result$ten_ten)^2+(test_result$real_y-test_result$ten_ten_y)^2)



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


p1=ggplot(data=test_result,aes(x=real,y=real_y,color=two_two_euc))+geom_point()+scale_colour_gradient(low="red",high="white")
p2=ggplot(data=test_result,aes(x=real,y=real_y,color=two_five_euc))+geom_point()+scale_colour_gradient(low="red",high="white")
p3=ggplot(data=test_result,aes(x=real,y=real_y,color=two_ten_euc))+geom_point()+scale_colour_gradient(low="red",high="white")

p4=ggplot(data=test_result,aes(x=real,y=real_y,color=five_two_euc))+geom_point()+scale_colour_gradient(low="red",high="white")
p5=ggplot(data=test_result,aes(x=real,y=real_y,color=five_five_euc))+geom_point()+scale_colour_gradient(low="red",high="white")
p6=ggplot(data=test_result,aes(x=real,y=real_y,color=five_ten_euc))+geom_point()+scale_colour_gradient(low="red",high="white")

p7=ggplot(data=test_result,aes(x=real,y=real_y,color=ten_two_euc))+geom_point()+scale_colour_gradient(low="red",high="white")
p8=ggplot(data=test_result,aes(x=real,y=real_y,color=ten_five_euc))+geom_point()+scale_colour_gradient(low="red",high="white")
p9=ggplot(data=test_result,aes(x=real,y=real_y,color=ten_ten_euc))+geom_point()+scale_colour_gradient(low="red",high="white")

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol=3)

#Error! Forget to remove NULL!
#warnings()
#warning：
#1: In cor(as.vector(p), as.vector(patch_sample)) : corvariance is 0
#2: In cor(as.vector(p), as.vector(patch_sample)) : corvariance is 0
#3: In cor(as.vector(p), as.vector(patch_sample)) : corvariance is 0
#4: In cor(as.vector(p), as.vector(patch_sample)) : corvariance is 0
#5: In cor(as.vector(p), as.vector(patch_sample)) : corvariance is 0
#6: In cor(as.vector(p), as.vector(patch_sample)) : corvariance is 0
#7: In cor(as.vector(p), as.vector(patch_sample)) : corvariance is 0



#Machine Learning



#Testing