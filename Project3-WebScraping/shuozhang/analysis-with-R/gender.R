setwd("~/Desktop/web scraping")
library(dplyr)
library(ggplot2)
library(dygraphs)
library(plotly)

event=read.csv('gender2.txt', header=T, stringsAsFactors = F, sep=',')
View(event)
#length(event$Event)
lst2=filter(event, grepl('Mixed', Event))
#length(lst2$Event)
lst1=filter(event, !grepl('Mixed', Event))

temp=strsplit(lst1$Event, split="'")
A=matrix(unlist(temp), ncol=2, byrow=TRUE)
sport=as.data.frame(A)
sport=cbind(sport, lst1$Sport)
colnames(sport)=c('gender', 'event', 'sport')
#View(sport)
sport%>%
  dplyr::group_by(gender)%>%
  dplyr::summarise(n=n())
# gender     n
# <fctr> <int>
#   1    Men   162
# 2  Women   132

#132 women events, 162 men events, 8 mixed events.

sport2=sport%>%
  dplyr::group_by(event)%>%
  dplyr::summarise(n=n())
common=dplyr::filter(sport2, n>=2)

uncommon=dplyr::filter(sport2, n==1)
uncommon1=merge(x=uncommon, y=sport, by='event', all.x = T)
#View(uncommon1)
uncommon1%>%
  dplyr::group_by(gender)%>%
  dplyr::summarise(n())
# gender   n()
# <fctr> <int>
#   1    Men    42
# 2  Women    15
# Of these, 57 events on the programme were gender exclusive 
# (i.e., there were medal opportunities for men but not for women and vice versa): 
# 42 events were open only to men (29% of men’s events); 
# and 15 events were open only to women (11.3% of women’s events). 
# Together, these exclusive events constituted  18.9% of the Olympic programme.

Event=c('men','men', 'women', 'women','mixed')
Category=c('only to men', 'equal to men and women', 'only to women', 'equal to men and women',
           'equal to men and women')
number=c(42, 120, 15, 117, 8 )
df=as.data.frame(Event,colnames=c('event'))
df$Category=Category
df$Number=number
df$Percent=df$Number/nrow(event)


ggplot(data=df, aes(x=reorder(Event, Percent), y=Percent, fill=Category))+
  geom_bar(stat = 'identity')+
  xlab('Event')+
  ylab('Percent')+
  ggtitle('Gender distribution of events in 2012 Olympics')+
  theme_bw()+
  theme(legend.position='bottom')
uncommonbysport=uncommon1%>%
  group_by(gender,sport)%>%
  dplyr::summarise(n=n())

ggplot(data=uncommonbysport, aes(x=reorder(sport,n), y=n, fill=gender))+
  geom_bar(stat='identity')+
  xlab('Sport')+
  ylab('Number of inequal events')+
  ggtitle('Sport of inequal gender in 2012 Olympics')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#competitor
total=read.csv('event.txt', header=T, stringsAsFactors = F, sep=',')
#View(total)
summary(total)
total$Women=as.numeric(gsub(',', '', total$Women))
total$Men=as.numeric(gsub(',','',total$Men))
total$Year=as.numeric(total$Year)
summary(total)
total=mutate(total, womenpercent=Women/sum(Men+Women), menpercent=Men/sum(Women+Men))

plot(total$Year, total$Men, type='l', col='red', xlab='Year', ylab='Percent',
     main='Gender distribution')
lines(total$Year, total$Women,col='blue')
legend("topleft", inset=.05, 
       c("Women","Men"),
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col=c('blue', 'red'), horiz=FALSE)

ggplot(data=total, aes(x=Year, y=percent, color=year))+
  geom_point()+
  geom_line()+
  xlab('Year')+
  ylab('Women percent')+
  ggtitle('Athletes distribution by gender')+
  theme_bw()+
  theme(legend.position='none')

people=read.csv('gender.txt', header=T, stringsAsFactors = F, sep=',')
summary(people)

people1=people%>%
  group_by(Gender)%>%
  dplyr::summarise(n=n())
# 1 Female  4234
# 2   Male  5223
df1=people%>%
  group_by(Gender, Gold)%>%
  dplyr::summarise(n=n())
#Gender  Gold     n
#   1  Female     1   252
# 2  Female     2    18
# 3  Female     3     3
# 4  Female     4     1
# 5  Female    NA  3960
# 6    Male     1   298
# 7    Male     2    13
# 8    Male     3     1
# 9    Male     4     1
# 10   Male    NA  4910
df2=people%>%
  group_by(Gender, Silver)%>%
  dplyr::summarise(n=n())
df3=people%>%
  group_by(Gender, Bronze)%>%
  dplyr::summarise(n=n())
Gender=c('Female','Female', 'Female', 'Female', 'Male', 'Male', 'Male', 'Male')
Metal=c('Gold', 'Silver', 'Bronze', 'No metal', 'Gold', 'Silver', 'Bronze', 'No metal')
Number=c(sum(filter(df1, Gender=='Female',Gold>=1)$n),
         sum(filter(df2, Gender=='Female',Silver>=1)$n),
         sum(filter(df3, Gender=='Female',Bronze>=1)$n),
         filter(people1, Gender=='Female')$n-sum(filter(df1, Gender=='Female',Gold>=1)$n)-sum(filter(df2, Gender=='Female',Silver>=1)$n)- sum(filter(df3, Gender=='Female',Bronze>=1)$n),
         sum(filter(df1, Gender=='Male',Gold>=1)$n),
         sum(filter(df2, Gender=='Male',Silver>=1)$n),
         sum(filter(df3, Gender=='Male',Bronze>=1)$n),
         filter(people1, Gender=='Male')$n-sum(filter(df1, Gender=='Male',Gold>=1)$n)-sum(filter(df2, Gender=='Male',Silver>=1)$n)- sum(filter(df3, Gender=='Male',Bronze>=1)$n)
         )

df4=as.data.frame(Gender, colnames=c('Gender'))
df4$Metal=Metal
df4$Number=Number
df4=mutate(df4,Percent=Number/sum(Number))
ggplot(data=df4, aes(x=Gender, y=Number, fill=Metal))+
  geom_bar(stat='identity')+
  xlab('Gender')+
  ylab('Number of metal')+
  ggtitle('Metal distribution by gender in 2012 Olympics')+
  theme_bw()
  
# age

totalage=select(people, Age)
totalage$Type=rep('All', nrow(totalage))

age1=filter(people, Gold>=1)%>%
  select(Age, Metal= Gold)
age1= mutate(age1, Type=rep('Gold',nrow(age1)))
summary(age1)
age2=filter(people, Silver>=1)%>%
  select(Age, Metal=Silver)
summary(age2)
age2=mutate(age2, Type=rep('Silver', nrow(age2)))
age3=filter(people, Bronze>=1)%>%
  select(Age,Metal= Bronze)
age3=mutate(age3, Type=rep('Bronze', nrow(age3)))
#summary(age3)

age=rbind(totalage, select(age1, Age, Type), select(age2, Age, Type), 
          select(age3, Age, Type))

g1=ggplot(data=age, aes(Age, color=Type))+
  geom_density(alpha=0.2)+
  xlab('Age')+
  ylab('Density')+
  ggtitle('Athletes age distribution in 2012 Olympics')+
  theme_bw()+
  theme(legend.position='bottom')
ggplotly(g1)
#avg age: around 25, min:14, max:71
people[is.na(people)]=0
people=mutate(people, Total=Gold+Silver+Bronze)
summary(people)

peoplebysport=filter(people, Total>=1)%>%
  group_by(Age, Sport)%>%
  dplyr::summarise(n=n())
g2=ggplot(peoplebysport, aes(x=Age, y=n, fill=Sport))+
  geom_bar(stat = 'identity')+
  xlab('Age')+
  ylab('Number of Medals')+
  ggtitle('Medal winners age distribution by sport in 2012 Olympics')+
  theme_bw()+
  theme(legend.position='bottom')
ggplotly(g2)
ggplot(filter(peoplebysport, Sport %in% c('Equestrianism', 'Gymnastics')), aes(x=Age, y=n, fill=Sport))+
  geom_bar(stat = 'identity')+
  xlab('Age')+
  ylab('Number of Medals')+
  ggtitle('Medal winners age distribution by sport in 2012 Olympics')+
  theme_bw()+
  theme(legend.position='bottom')

# You can see that very young medalists (15 years old), 
# and the very old people medalists (56 year old) 
# older medalists (late 30s-over 40) tend to win fewer total medals 
# than medalists within the age-range “sweet spot” of late teens-early 30s.

#Some sports, like equestrianism, tend to have older athletes winning medals,
# whereas a sport like gymnastics most commonly has a peak age-range 
# of early-to-late teens  and early20s.

# sport
peoplebysport1=filter(people, Gold>=1)%>%
  group_by(Sport, Gold)%>%
  dplyr::summarise(n=n())%>%
  select(Sport, n)%>%
  mutate(Type=rep('Gold', nrow(.)))
peoplebysport2=filter(people, Silver>=1)%>%
  group_by(Sport, Silver)%>%
  dplyr::summarise(n=n())%>%
  select(Sport, n)%>%
  mutate(Type=rep('Silver', nrow(.)))
peoplebysport3=filter(people, Bronze>=1)%>%
  group_by(Sport, Bronze)%>%
  dplyr::summarise(n=n())%>%
  select(Sport, n)%>%
  mutate(Type=rep('Bronze', nrow(.)))
peoplebysport=rbind(peoplebysport1, peoplebysport2, peoplebysport3)


g3=ggplot(peoplebysport, aes(x=reorder(Sport, n), y=n, fill=Type))+
  geom_bar(stat = 'identity')+
  xlab('Sport')+
  ylab('Number of medal')+
  ggtitle('Medal distribution by sport in 2012 Olympics')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='bottom')
ggplotly(g3)

# Now we can see which sports produce the most medals.
# Athletics is number one, followed by swimming, rowing, and football.
#So if you’re an aspiring Olympian,but you’re not quite sure what sport 
# to train for, you’ll increase your chances of medaling if you choose athletics.


  


  















  
