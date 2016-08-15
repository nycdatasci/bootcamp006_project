library(ggplot2)
library(dplyr)
library(ggthemes)
library(maps)
library(grid)
library(gridExtra)
library(corrplot)
library(zipcode)
data("zipcode")


dat = read.csv("port_area_house.csv")[,-1]
school = read.csv("school.csv")
dat <- unique(dat)

dat$sold_date <- as.Date(dat$sold_date ) 
dat$price=as.numeric(dat$price)
dat$beds=as.numeric(dat$beds)
dat$baths = as.numeric(dat$baths)
dat$price = as.numeric(dat$price)
dat$price_sqft = as.numeric(dat$price_sqft)
dat$sqft = as.numeric(dat$sqft)
dat$elementary_rating = as.numeric(dat$elementary_rating)
dat$middle_rating = as.numeric(dat$middle_rating)
dat$high_rating = as.numeric(dat$high_rating)
dat7 <- dat[dat$sold_date>='2015-08-01',]

dat8 <- dat7 %>% group_by(.,sold_date,zip) %>% summarise(.,mean_price= median(price_sqft))
ggplot(data = dat8, aes(x=sold_date, y=mean_price))+
  geom_line()+
  facet_wrap(~zip)


dat1 <- dat %>% group_by(.,sold_date) %>% summarise(.,mean_price= median(price_sqft))


dim(dat)
# total 23086 observations for the past three years, and 14 variables.
break1 = c(2013-8-1,2014-8-1,2015-8-1,2016-8-1)
ggplot(dat1,aes(x=sold_date, y=mean_price))+
  geom_line()+
  xlab("Sold Year")+
  ylab("Median Price per Square Foot")+
  ylim(50,300)+
  ggtitle("Median House Price per SqFt in Portland Area for Past Three Years")+
  geom_smooth()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        plot.title = element_text(size= 14, vjust = 2, face="bold"),
        axis.title = element_text(size= 12),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
    
ggsave("price_sqft_portland_median.png")

dat_zip <- dat %>% group_by(., sold_date, zip) %>% summarise(.,median = median(price))



dat$zip <- factor(dat$zip, 
                      levels = c("97202","97206","97212","97214",
                                 "97217","97219","97220","97229",
                                 "97232","97233","98607","98661",
                                 "98663","98664","98665","98682",
                                 "98683","98684","98685","98686"
                                 ))



ggplot(dat, aes(x=zip, y=price_sqft,fill=city))+
  geom_boxplot()+
  xlab("Zipcodes")+
  ylab("Price ($ per Square Foot)")+
  ylim(0,650)+
  ggtitle("House Price/SqFt in Portland Metro Area for Past Three Years")+
  geom_smooth()+
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 14, vjust = 2, face="bold"),
        axis.title = element_text(size= 12),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("Port_Vanc_three_years.pdf")






dat$elementary_rating <- round(dat$elementary_rating,1)
dat$middle_rating <- round(dat$middle_rating,1)
dat$high_rating <- round(dat$high_rating,1)

dat$elementary_rating <- factor(dat$elementary_rating)
dat$middle_rating <- factor(dat$middle_rating)
dat$high_rating <- factor(dat$high_rating)

str(dat)

p1 <- ggplot(dat, aes(x=elementary_rating, y=price_sqft,fill=city))+
  geom_boxplot()+
  facet_wrap(~city)+
  xlab("Elementary School Rating")+
  ylab("Price ($ per Square Foot)")+
  ylim(0,650)+
  ggtitle("House Price/SqFt VS Elementary School Rating") +
  geom_smooth()+
  theme(legend.position = "None",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 12, vjust = 2, face="bold"),
        axis.title = element_text(size= 10),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("house_elementary.pdf")

p2 <- ggplot(dat, aes(x=middle_rating, y=price_sqft,fill=city))+
  geom_boxplot()+
  facet_wrap(~city)+
  xlab("Middle School Rating")+
  ylab("Price ($ per Square Foot)")+
  ylim(0,650)+
  ggtitle("House Price/SqFt VS Middle School Rating") +
  geom_smooth()+
  theme(legend.position = "None",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 12, vjust = 2, face="bold"),
        axis.title = element_text(size= 10),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("house_middle.pdf")

p3 <- ggplot(dat, aes(x=high_rating, y=price_sqft,fill=city))+
  geom_boxplot()+
  facet_wrap(~city)+
  xlab("High School Rating")+
  ylab("Price ($ per Square Foot)")+
  ylim(0,650)+
  ggtitle("House Price/SqFt VS High School Rating") +
  geom_smooth()+
  theme(legend.position = "None",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 12, vjust = 2, face="bold"),
        axis.title = element_text(size= 10),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("house_high.pdf")
grid.arrange(p1, p2, p3, ncol=2, nrow =2)
ggsave("House_school_all_three_years.png")
ggsave("House_school_all_three_years.pdf")




####dat2 the latest one year date
dat2 <- dat[dat$sold_date>='2015-08-01',]

ggplot(dat2, aes(x=elementary_rating, y=price_sqft,fill=city))+
  geom_boxplot()+
  xlab("Elementary School Rating")+
  ylab("Price ($ per Square Foot)")+
  ylim(0,650)+
  ggtitle("House Price/SqFt VS Elementary School Rating") +
  geom_smooth()+
  theme(legend.position = "None",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 12, vjust = 2, face="bold"),
        axis.title = element_text(size= 10),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("house_elementary_1year.pdf")

p5 <- ggplot(dat2, aes(x=middle_rating, y=price_sqft,fill=city))+
  geom_boxplot()+
  facet_wrap(~city)+
  xlab("Middle School Rating")+
  ylab("Price ($ per Square Foot)")+
  ylim(0,650)+
  ggtitle("House Price/SqFt VS Middle School Rating") +
  geom_smooth()+
  theme(legend.position = "None",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 12, vjust = 2, face="bold"),
        axis.title = element_text(size= 10),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("house_middle_1year.pdf")

p6 <- ggplot(dat2, aes(x=high_rating, y=price_sqft,fill=city))+
  geom_boxplot()+
  facet_wrap(~city)+
  xlab("High School Rating")+
  ylab("Price ($ per Square Foot)")+
  ylim(0,650)+
  ggtitle("House Price/SqFt VS High School Rating") +
  geom_smooth()+
  theme(legend.position = "None",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 12, vjust = 2, face="bold"),
        axis.title = element_text(size= 10),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("house_high_1year.pdf")
grid.arrange(p4, p5, p6, ncol=2, nrow =2)
ggsave("House_school_all_1year.png")
ggsave("House_school_all_1year.pdf")

dat3 <- dat2[dat2$zip=='98607',]

ggplot(dat3, aes(x=sqft, y=price_sqft), fill='blue')+
  geom_point()+
  geom_smooth()+
  xlab("House Area (SqFt)")+
  ylab("Price ($ per Square Foot)")+
  ggtitle("Price/SqFt VS House Area at 98607\n (From Aug 2015 to Aug 2016)")+
  theme(legend.position = "None",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 12, vjust = 2, face="bold"),
        axis.title = element_text(size= 10),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 

ggsave('$sqft_area.pdf')

model = lm(price_sqft ~ sqft, data = dat3)
summary(model)
confint(model)
plot(model)
cor(dat3$price_sqft,dat3$sqft)


dat4 <- dat3[,c(1,2,5,6,7)]

             #,dat3$sqft,dat3$price],dat3$price_sqft

plot(dat4)

dat5 <- dat2[,c(5,7,10,11,12,13,14)]

#plot(dat5)

ggplot(dat5, aes(x=elementary_rating, y=price,fill=city))+
  geom_boxplot()+
  xlab("Elementary School Rating")+
  ylab("Price ($)")+
  ylim(0,1.5*10**6)+
  ggtitle("House Price in Portland Metro Area\n (Aug 2015 to Aug 2016)")+
  geom_smooth()+
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 14, vjust = 2, face="bold"),
        axis.title = element_text(size= 12),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("price_elementary_rating_oneyear_Portland_.pdf")

ggplot(dat5, aes(x=middle_rating, y=price,fill=city))+
  geom_boxplot()+
  xlab("Middle School Rating")+
  ylab("Price ($)")+
  ylim(0,1.5*10**6)+
  ggtitle("House Price in Portland Metro Area\n (Aug 2015 to Aug 2016)")+
  geom_smooth()+
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 14, vjust = 2, face="bold"),
        axis.title = element_text(size= 12),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("price_middle_rating_oneyear_Portland.pdf")


ggplot(dat5, aes(x=high_rating, y=price,fill=city))+
  geom_boxplot()+
  xlab("High School Rating")+
  ylab("Price ($)")+
  ylim(0,1.5*10**6)+
  ggtitle("House Price in Portland Metro Area\n (Aug 2015 to Aug 2016)")+
  geom_smooth()+
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 14, vjust = 2, face="bold"),
        axis.title = element_text(size= 12),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  ) 
ggsave("price_high_rating_oneyear_Portland.pdf")

ggplot(dat5, aes(x=high_rating, y=sqft,fill=city))+
  geom_boxplot()+
  xlab("High School Rating")+
  ylab("Price ($)")+
  ylim(0,8000)+
  ggtitle("House Price in Portland Metro Area (Aug 2015 to Aug 2016)")+
  geom_smooth()+
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size= 14, vjust = 2, face="bold"),
        axis.title = element_text(size= 12),
        panel.margin = unit(0.1,"in"),
        strip.text = element_text(face = c("italic","bold"), size = 9, colour= "blue3")
  )

dat6 <- dat2[,c(10,11,12)]

ggplot(dat6,aes(x=dat6$elementary_rating, y=dat6$middle_rating))+
  geom_point()

plot(dat6)
ggsave('school_rating.pdf')

cor(as.numeric(dat6$elementary_rating),as.numeric(dat6$middle_rating))
cor(as.numeric(dat6$high_rating),as.numeric(dat6$middle_rating))
cor(as.numeric(dat6$elementary_rating),as.numeric(dat6$high_rating))

#dat7 <- dat2[,c(2,3,6,7,8,11,12,13,15)]

dat8 <- dat7[,c(2,3,6,7,8,11,12,13)]
names(dat8$elementary_rating) = "elem_rate"
names(dat8)
corrplot(cor(dat8), order = "hclust")
ggsave("correlation.pdf")
ggsave("correlation.png")

########################################
####### How to plot map? ###############
########################################
#dat9 <- dat %>% group_by(.,zip) %>%
#  summarise(., median_price= median(price))

#dat_map <- merge(zipcode,dat7, by = 'zip')
#ggplot(data = zipcode, aes(x=latitude,y=longitude))+
#  geom_polygon()


  #geom_polygon(data=dat_map, aes(x=longitude, y=latitude, colour=median_price))
 # geom_point(data=dat_map, aes(x=longitude, y=latitude, colour=price))+
#  coord_map()





