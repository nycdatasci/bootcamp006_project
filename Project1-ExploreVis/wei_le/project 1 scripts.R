?strsplit
install.packages("stringr")

package 'stringr' successfully unpacked and MD5 sums checked

engprelead = read.csv('', stringsAsFactors = F)

The downloaded binary packages are in
C:\Users\Le Wei\AppData\Local\Temp\RtmpUJtBTU\downloaded_packages

library(stringr)
FT1 = str_split_fixed(as.character(engprelead$FT),'-',2)
  
FT1 = str_split_fixed(as.character(engprelead$FT),'-',2)

> engprelead=cbind(engprelead,FT1)

#below is trying str_split
fruits
[1] "apples and oranges and pears and bananas" "pineapples and mangos and guavas"        
str_split(fruits, " and ", n = 3)
[[1]]
[1] "apples"            "oranges"           "pears and bananas"

[[2]]
[1] "pineapples" "mangos"     "guavas"    


str_split(fruits, " and ", n = 2)
[[1]]
[1] "apples"                        "oranges and pears and bananas"

[[2]]
[1] "pineapples"        "mangos and guavas"


str_split(fruits, " and ", n = 5)
[[1]]
[1] "apples"  "oranges" "pears"   "bananas"

[[2]]
[1] "pineapples" "mangos"     "guavas"    

> 
  unlist(str_split(fruits, " and ", n = 5))
[1] "apples"     "oranges"    "pears"      "bananas"    "pineapples" "mangos"     "guavas"    
> #tring above stopped
  
  #the below is about how to change delete colume and delete duplicated column and how to change the columnname
  engprelead$`1`=NULL
# engprelead
#Date         Team.1         Team.2  FT  HT as.numeric(FT1) 2 1 2
#1   2013-08-17        Arsenal    Aston Villa 1-3 1-1               1 3 1 3
#2   2013-08-17      Liverpool          Stoke 1-0 1-0               1 0 1 0
#3   2013-08-17        Norwich        Everton 2-2 0-0               2 2 2 2
#4   2013-08-17     Sunderl

engprelead<-engprelead[,1:8]
View(engprelead)
View(engprelead)
View(engprelead)
colnames(engprelead)
# [1] "Date"            "Team.1"          "Team.2"          "FT"              "HT"              "as.numeric(FT1)"
# [7] "2"               "1"              
engprelead<-engprelead[,-6]
colnames(engprelead)
# [1] "Date"   "Team.1" "Team.2" "FT"     "HT"     "2"      "1"     
colnames(engprelead)[c(6,7)]
#[1] "2" "1"
colnames(engprelead),-c("score1","score2")
#Error: unexpected ',' in "colnames(engprelead),"
colnames(engprelead)<-c("score1","score2")
View(engprelead)
colnames(engprelead)<-c("Date","Team.1","Team.2","FT","HT","score1","score2") #this is how to change column name
View(engprelead)

read.csv('engprelead.txt', stringsAsFactors = F)
# the above code is w=for after all the data.frame was done,  after saving csv by a doc name, ????????????without string as factors

a=as.vector(engprelead$score1)
b=as.vector(engprelead$score2)

EPL=read.csv('EPL.csv', stringsAsFactors = F)
Teamwon<- ifelse(EPL$score1 > EPL$score2, EPL$Team.1, EPL$Team.2)
winning= ifelse(EPL$score1 > EPL$score2, EPL$Team.1, ifelse(EPL$score1 == EPL$score2,
                                                            +                                      'draw', EPL$Team.2))
head(winning)
"Arsenal"    "Stoke"      "draw"       "Sunderland" "Swansea"    "West Brom" 
str(winning)
chr [1:760] "Arsenal" "Stoke" "draw" "Sunderland" "Swansea" "West Brom" "Cardiff" "Hull" ...
D=cbind(EPL,winning)
write.csv(d,file="EPL14.csv",row.names = F)
EPL14=read.csv("EPL14.csv", stringsAsFactors = FALSE)
summarise(EPL14, n_distinct(Team.1))
#above calc how many teams they have

library(dplyr)
Arsenallost=filter(EPL14, lost == "Arsenal")
Mancitylost=filter(EPL14, lost == "Man City")
Chelsealost=filter(EPL14, lost == "Chelsea")
Liverpoollost=filter(EPL14, lost == "Liverpool")
nrow(Arsenallost)
nrow(Mancitylost)
nrow(Chelsealost)
nrow(Liverpoollost)

teamwin= ifelse(EPL14$score1 < EPL14$score2, EPL$Team.1, ifelse(EPL14$score1 == EPL14$score2,'draw', EPL14$Team.2)
)
cbind(EPL14,teamwin)
EPL1314=cbind(EPL14,teamwin)
Arsenalwin=filter(EPL1314, teamwin == "Arsenal")
nrow(Arsenalwin)
Mancitywin=filter(EPL1314, teamwin == "Man City")
nrow(Mancitywin)
Chelseawin=filter(EPL1314, teamwin == "Chelsea")
nrow(Chelseawin)
Liverpoolwin=filter(EPL1314, teamwin == "Liverpool")
nrow(Liverpoolwin)

filter(EPL1314, lost=="draw" & Team.1=="Arsenal" & Team.2=="Arsenal")
filter(EPL1314, lost=="draw" & Team.1=="Arsenal" |lost== "draw" & Team.2=="Arsenal") 
m=filter(EPL1314, lost=="draw" & Team.1=="Man City" |lost== "draw" & Team.2=="Man City")
 
nrow(m)
10
Ar=filter(EPL1314, lost=="draw" & Team.1=="Arsenal" |lost== "draw" & Team.2=="Arsenal") nrow(Ar)
14
Li=filter(EPL1314, lost=="draw" & Team.1=="Liverpool" |lost== "draw" & Team.2=="Liverpool")
nrow(Li)
12
che=filter(EPL1314, lost=="draw" & Team.1=="Chelsea" |lost== "draw" & Team.2=="Chelsea")
nrow(che)
14

Hostresult = ifelse(EPL1314$score1 > EPL1314$score2, "lost", ifelse(EPL1314$score1 == EPL$score2,'draw', "win"))
head(Hostresult)
#"lost" "win"  "draw" "lost" "lost" "lost"
EPL14=cbind(EPL1314, Hostresult)
ggplot(data = EPL1314, aes(x=Team.1)) + geom_bar(aes(fill = Hostresult), position = "dodge")
guestresult = ifelse(EPL14$score1 > EPL14$score2, "win", ifelse(EPL14$score1 == EPL$score2,'draw', "lost"))
head(guestresult)
# "win"  "lost" "draw" "win"  "win"  "win" 
EPL1414=cbind(EPL14, guestresult)
ggplot(data = EPL1414, aes(x=Team.2)) + geom_bar(aes(fill = guestresult), position = "dodge")
ggplot(data = EPL1414, aes(x= Team.1)) + geom_bar(aes(fill = Hostresult))
ggplot(data = EPL1414,aes(x = Team.2)) + geom_bar(aes(fill = guestresult))
HF = str_split_fixed((EPL1414$HT),"-",2)
head(HF)
EPL1415=cbind(EPL1414,HF)
colnames(EPL1415)<-c("Date","Team.1","Team.2","FT","HT","score1","score2" ,"lost","teamwin", "Hostresult", "guestresult","halfscore1", "halfscore2")
head(EPL1415)
EPL141515=write.csv(EPL1415,file="EPL141515.csv",row.names = F)
read.csv('EPL141515.csv', stringsAsFactors = F)
EPL141515
EPL141515=read.csv('EPL141515.csv', stringsAsFactors = F)
str(EPL141515)
ifelse(EPL141515$halfscore1<=EPL141515$halfscore2 & EPL141515$score2 >= EPL141515$score1, "come back", "no")
comeback=ifelse(EPL141515$halfscore1<EPL141515$halfscore2 & EPL141515$score2 > EPL141515$score1, "come back", "no")
EPL141516=cbind(EPL141515, comeback)
head(EPL141516)
ggplot(data = EPL141516, aes(x = Team.1)) + geom_bar(aes(fill = comeback), position = "dodge")
ggplot(data = EPL141516, aes(x = Team.2)) + geom_bar(aes(fill = comeback), position = "dodge")
ifelse(EPL141515$halfscore1<=EPL141515$halfscore2 & EPL141515$score2 > EPL141515$score1, "goodsecondhalf", "no")
str(Engpreleg)
ggplot(data = Engpreleg, aes(x = Team.1)) + geom_bar(aes(fill = goodsechalf), position = "dodge")
ggplot(data = Engpreleg, aes(x = Team.2)) + geom_bar(aes(fill = goodsechalf), position = "dodge")
filter(Engpreleg, Team.1== 'Arsenal' & score2) #have question
A=filter(Engpreleg, Team.1== 'Arsenal' & score2)
sum(A$score2)
A=filter(Engpreleg, Team.1== 'Arsenal' & score2)
 sum(A$score2)
72
MC=filter(Engpreleg, Team.1== 'Man City' & score2)
 sum(MC$score2)
126
ch=filter(Engpreleg, Team.1== 'Chelsea' & score2)
sum(ch$score2)
 86
liv=filter(Engpreleg, Team.1== 'Liverpool' & score2)
 sum(liv$score2)
 106
 A1=filter(Engpreleg, Team.2== 'Arsenal' & score1)
 sum(A1$score1)
64
 MC1=filter(Engpreleg, Team.2== 'Man City' & score1)
 sum(MC1$score2)
46
 ch1=filter(Engpreleg, Team.2== 'Chelsea' & score1)
 sum(ch1$score1)
 56
liv1=filter(Engpreleg, Team.2== 'Liverpool' & score1)
sum(liv1$score1)
96
distinct(Engpreleg, Team.1)
home_goal <- group_by(Engpreleg, Team.1) %>% summarise(homegoal = sum(score2))
away_goal <- group_by(Engpreleg, Team.2) %>% summarise(awaygoal1 = sum(score1))
total = home_goal$homegoal + away_goal1$awaygoal
engpreleg1<-engpreleg1[,-1]
g <- ggplot( data = engpreleg1, aes(x = total)) 
g + geom_histogram(aes(fill = Team.1), binwidth = 5)
g <- ggplot( data = engpreleg1, aes(x = total)) 
g + geom_histogram(binwidth = 1)
g + geom_histogram(binwidth = 5)
standings=c("Man City", "Liverpool", "Chelsea", "Arsenal", "Everton", "Tottenham", "Man United", "Southampton", "Stoke", "Newcastle", "Crystal Palace","Swansea", "West Ham", "Sunderland","Aston Villa", "Hull", "West Brom", "Norwich", "Fulham", "Cardiff")
Catgory=rep(c("Topteams", "Midup", "Middown", "worstteams"), c(5,5,5,5))
stcat=cbind(standings, Catgory)
dplyr::left_join(engpreleg1, stcat, by = "Team.1", copy = T)
g + geom_histogram(binwidth = 5) + facet_wrap( ~ categoty)
g <- ggplot( data = Eng, aes(x = homegoal)) 
g + geom_histogram(binwidth = 5) + facet_wrap( ~ categoty)
g <- ggplot( data = Eng, aes(x = awaygoal)) 
g + geom_histogram(binwidth = 5) + facet_wrap( ~ categoty)
filter(Engpreleg, Team.1=="Arsenal" & Hostresult=="win")
summarise(group_by(Engpreleg, Team.1), sum(guestresult=="win"))
ggplot(data = Engpreleg, aes(x = Team.1)) + geom_bar(aes(fill = goodsechalf), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ggplot(data = Engpreleg, aes(x = Team.1)) + geom_bar(aes(fill = goodsechalf), position = "dodge", width=0.5)
 ggplot(data = Engpreleg, aes(x = Team.1)) + geom_bar(aes(fill = goodsechalf), position = "dodge", width=0.5)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ggplot(data = Engpreleg, aes(x = Team.2)) + geom_bar(aes(fill = goodsechalf), position = "dodge", width=0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ggplot(data = Engpreleg, aes(x=Team.2)) + geom_bar(aes(fill = guestresult), position = "dodge")
ggplot(data = Engpreleg, aes(x=Team.2)) + geom_bar(aes(fill = guestresult), position = "dodge", width=0.5)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ggplot(data = Engpreleg, aes(x=Team.1)) + geom_bar(aes(fill = hostresult), position = "dodge", width=0.5)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ggplot(data = Engpreleg, aes(x=Team.1)) + geom_bar(aes(fill = Hostresult), position = "dodge", width=0.5)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
 g + geom_histogram(binwidth = 5) + facet_wrap( ~ Catgory) + xlab("Total goal")
 g + geom_histogram(binwidth = 5) + facet_wrap( ~ Catgory) + ylab("Total Team")
 