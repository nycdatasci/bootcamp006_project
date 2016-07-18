
library(ggplot2)
library(maps)
library(dplyr)
library(gridExtra)
library(grid)

####MAPS

project1=read.csv('C:/Users/Marina/Documents/Data Science/project.csv',header=TRUE, sep="," )

group<-summarise(group_by(project1,region), friends=mean(friends,na.rm=T), family=mean(family, na.rm=T),work=mean(work,na.rm=T),time=mean(time,na.rm=T), politics=mean(politics,na.rm=T), religion=mean(religion,na.rm=T))

map <- map_data("world")


m<-left_join(map,group, by="region")
p <- ggplot(project1, aes(fill = family))
map1 <- p+ geom_polygon( data=m, aes(x=long, y=lat, group = group, fill=family),colour="grey0")+scale_fill_gradient(name="FAMILY",low = "white", high ="deeppink1", space = "Lab",na.value = "grey98", guide = "colorbar")+theme(legend.position="top",legend.key.size = unit(.3, "cm"),legend.title=element_text(size=12))


q <- ggplot(project1, aes(fill = friends))
map2 <- q+ geom_polygon( data=m, aes(x=long, y=lat, group = group, fill=friends),colour="grey0")+scale_fill_gradient(name="FRIENDS", low = "white", high ="darkorchid1", space = "Lab",na.value = "grey98", guide = "colorbar")+theme(legend.position="top",legend.key.size = unit(.3, "cm"),legend.title=element_text(size=12))


f <- ggplot(project1, aes(fill = time))
map3 <- f+ geom_polygon( data=m, aes(x=long, y=lat, group = group, fill=time),colour="grey0")+scale_fill_gradient(name="LEISURE TIME",low = "white", high ="firebrick1", space = "Lab",na.value = "grey98", guide = "colorbar")+theme(legend.position="top",legend.key.size = unit(.3, "cm"),legend.title=element_text(size=12))


g <- ggplot(project1, aes(fill = work))
map4 <- g+ geom_polygon( data=m, aes(x=long, y=lat, group = group, fill=work),colour="grey0")+scale_fill_gradient(name="WORK",low = "white", high ="turquoise1", space = "Lab",na.value = "grey98", guide = "colorbar")+theme(legend.position="bottom")+theme(legend.position="top",legend.key.size = unit(.3, "cm"),legend.title=element_text(size=12))


r <- ggplot(project1, aes(fill = religion))
map5 <- r+ geom_polygon( data=m, aes(x=long, y=lat, group = group, fill=religion),colour="grey0")+scale_fill_gradient(name="RELIGION",low = "white", high ="chartreuse1", space = "Lab",na.value = "grey98", guide = "colorbar")+theme(legend.position="top",legend.key.size = unit(.3, "cm"),legend.title=element_text(size=12))



h <- ggplot(project1, aes(fill = politics))
map6 <- h+ geom_polygon( data=m, aes(x=long, y=lat, group = group, fill=politics),colour="grey0")+scale_fill_gradient(name="POLITICS",low = "white", high ="dodgerblue1", space = "Lab",na.value = "grey98", guide = "colorbar")+theme(legend.position="top",legend.key.size = unit(.3, "cm"),legend.title=element_text(size=12))



map_final=grid.arrange(map1, map2,map3,map4, map5,map6, ncol=3,nrow=2,top=textGrob("VALUES AROUND THE GLOB", gp=gpar(fontsize=17,font=2)))



#######BAR CHARTS

##differences by sex

library(ggplot2)
library(maps)
library(dplyr)
library(gridExtra)
library(grid)

project1=read.csv('C:/Users/Marina/Documents/Data Science/project.csv',header=TRUE, sep="," )

group<-summarise(group_by(project1,region), friends=mean(friends,na.rm=T), family=mean(family, na.rm=T),work=mean(work,na.rm=T),time=mean(time,na.rm=T), politics=mean(politics,na.rm=T), religion=mean(religion,na.rm=T))

map <- map_data("world")

temp = project1[complete.cases(project1), ]
temp = temp[!temp$sex==" ", ]
g<-summarise(group_by(temp,sex), friends=mean(friends,na.rm=T), family=mean(family, na.rm=T),work=mean(work,na.rm=T),time=mean(time,na.rm=T), politics=mean(politics,na.rm=T), religion=mean(religion,na.rm=T))

plot1 = ggplot(g, aes(x = sex, y=family, fill=sex)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("violetred1","darkorchid1"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Family")

plot2 = ggplot(g, aes(x =sex, y=friends,fill=sex)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("violetred1","darkorchid1"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Friends")

plot3 = ggplot(g, aes(x = sex, y=time,fill=sex)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("violetred1","darkorchid1"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Leisure Time")

plot4 = ggplot(g, aes(x = sex, y=work, fill=sex)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("violetred1","darkorchid1"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Work")

plot5 = ggplot(g, aes(x = sex, y=religion, fill=sex)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("violetred1","darkorchid1"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Religion")

plot6 = ggplot(g, aes(x = sex, y=politics, fill=sex)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("violetred1","darkorchid1"), guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Politics")


grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6, ncol=3,nrow=2,top=textGrob("VALUES BY SEX", gp=gpar(fontsize=17,font=2)))


##differences by happiness

temp = project1[complete.cases(project1), ]
temp = temp[!temp$happy==" ", ]
g<-summarise(group_by(temp,happy), friends=mean(friends,na.rm=T), family=mean(family, na.rm=T),work=mean(work,na.rm=T),time=mean(time,na.rm=T), politics=mean(politics,na.rm=T), religion=mean(religion,na.rm=T))

plot1 = ggplot(g, aes(x = happy, y=family, fill=happy)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("orchid1","pink3"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(,size=12,face="bold"))+labs(x=NULL,y="Family")

plot2 = ggplot(g, aes(x =happy, y=friends,fill=happy)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("orchid1","pink3"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Friends")

plot3 = ggplot(g, aes(x = happy, y=time,fill=happy)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("orchid1","pink3"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Leisure Time")

plot4 = ggplot(g, aes(x = happy, y=work, fill=happy)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("orchid1","pink3"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Work")

plot5 = ggplot(g, aes(x = happy, y=religion, fill=happy)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("orchid1","pink3"),guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Religion")

plot6 = ggplot(g, aes(x = happy, y=politics, fill=happy)) + geom_bar(stat = 'identity',width=.7)+ scale_fill_manual(values=c("orchid1","pink3"), guide = FALSE)+theme(axis.title=element_text(size=15,face="bold"),axis.text.x = element_text(size=12,face="bold"))+labs(x=NULL,y="Politics")



grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6, ncol=3,nrow=2,top=textGrob("VALUES BY HAPPINESS", gp=gpar(fontsize=17,font=2)))



######### lines

temp = project1[complete.cases(project1), ]




ggplot(temp, aes(year, y = Values, color = values)) + 
  geom_smooth(aes(y = friends, col = "family"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = family, col = "friends"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = religion, col = "leisure time"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = time, col = "work"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = politics, col = "religion"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = work, col = "politics"),method="lm", se=FALSE,size=1.2)+
  scale_color_manual(values=c("deeppink1","darkorchid1","firebrick1","turquoise1","chartreuse1","dodgerblue1"))+
  labs(x="Year",y="Values")+theme(axis.title=element_text(size=15,face="bold"),legend.text=element_text(size=12),legend.title=element_text(size=14,face="bold"),plot.title = element_text(size = 15, face = "bold"))+
  ggtitle("CHANGES IN VALUES THROUGHOUT YEARS")

ggplot(temp, aes(income, y = Values, color = values)) + 
  geom_smooth(aes(y = friends, col = "family"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = family, col = "friends"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = religion, col = "leisure time"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = time, col = "work"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = politics, col = "religion"),method="lm", se=FALSE,size=1.2)+
  geom_smooth(aes(y = work, col = "politics"),method="lm", se=FALSE,size=1.2)+
  scale_color_manual(values=c("deeppink1","darkorchid1","firebrick1","turquoise1","chartreuse1","dodgerblue1"))+
  labs(x="Income",y="Values")+theme(axis.title=element_text(size=15,face="bold"),legend.text=element_text(size=12),legend.title=element_text(size=14,face="bold"),plot.title = element_text(size = 15, face = "bold"))+
  ggtitle("CHANGES IN VALUES BY INCOME GROUP")

