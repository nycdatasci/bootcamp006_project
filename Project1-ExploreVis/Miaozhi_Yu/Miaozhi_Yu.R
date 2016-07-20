setwd("F:/Miaozhi/Academic/Data_Science/Bootcamp/Project")
mydata = read.csv('Group3_data.csv')
names(mydata)
head(mydata)
summary(mydata)

install.packages(c("raster","maptools","png","grid","dplyr","ggplot2","devtools","ggmap","plyr"))
install.packages('cowplot')
try(detach("package:ggmap", unload = TRUE), silent = TRUE)
# install.packages('devtools')
library(devtools)
install_github("fdetsch/ggmap", ref = "develop")
## load required packages
# install.packages('maptools')
library(png)
library(grid)
library(dplyr)
library(ggplot2)
library(ggmap)
library(raster)
library(maptools)
library(plyr)
library(cowplot)

img = readPNG('Picture1.png')
montesino_park <- ggmap2raster(img)
g=rasterGrob(img)
p <- ggmap:::rggbplot(g, npix = ncell(g))

g4 = ggplot(data=mydata,aes(x=X,y=Y))+
  geom_point(position='jitter')+
  scale_x_continuous(limits=c(1,9),breaks = 1:9,expand = c(0,0))+
  scale_y_reverse(limits=c(9,1),breaks = 9:1,expand=c(0,0))+
  theme_bw()+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())


#staring to analyse data

mydata_new = mydata[mydata$area>0,]
summary(mydata$area)
head(mydata_new)
names(mydata_new)

fit  = glm(log(area+1)~FFMC+DMC+DC+ISI+temp+RH+wind+rain,data=mydata_new,family=Gamma())
summary(fit)

summary(mydata_new$area)
range = ifelse(mydata_new$area<2.14,'small',ifelse(mydata_new$area>15.42,'large','median'))
season = ifelse(mydata_new$month %in% c(11,12,1),'Winter',ifelse(mydata_new$month %in% c(2,3,4,5),'Spring',
                                                                 ifelse(mydata_new$month %in% c(6,7,8),'Summer','Fall')))

descriptive = mutate(mydata_new,range=as.factor(range),season=season)

g = ggplot(data=mydata_new,aes(x=range,fill=range))
g+geom_bar(aes(fill=range))+labs(title='Bar Chart of different size of forest fire')
gboxplot=ggplot(data=descriptive,aes(x=range,y=area))+geom_boxplot()+ggtitle('Box Plot of the area')
gboxplot
gboxplot2=ggplot(data=descriptive[descriptive$range %in% c("median","small"),],aes(x=range,y=area))+geom_boxplot()+ggtitle('Box Plot of the area')
gboxplot2

#The seasons need to be ordered 
gseason = ggplot(data = descriptive,aes(x=season))+geom_bar(aes(fill=season))+ggtitle('Bar Chart of Seaon')
gseason

g1 = ggplot(data=mydata_new, aes(x=RH,y=area))
g1+geom_point(aes(color=range),position = 'jitter')+labs(title='Fire area vs RH', x = 'RH',y='Fire area')
g1+geom_smooth(data=mydata_new,method='loess', se=FALSE)+ggtitle('Smooth trend of area and RH')



g2=ggplot(data=mydata_new,aes(x=ISI,y=area))
g2+geom_point(position = 'jitter',aes(color=range))+labs(title='Fire area vs ISI', x = 'ISI',y='Fire area')
g2+geom_smooth(data=mydata_new,method='loess', se=FALSE)+ggtitle('Smooth trend of area and ISI')


g3=ggplot(data=mydata_new,aes(x=DMC,y=area))
ggplot(data=mydata_new,aes(x=rain,y=area))+geom_point(position = 'jitter',aes(color=range))+labs(title='Fire area vs DMC', x = 'DMC',y='Fire area')
g2+geom_smooth(data=mydata_new,method='loess', se=FALSE)+ggtitle('Smooth trend of area and DMC')
