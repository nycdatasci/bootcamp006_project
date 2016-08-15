setwd("~/Desktop/web scraping")
library(ggplot2)
library(data.table)
library(dygraphs)
library(dplyr)
swimming=read.csv('swimming.txt', header=T, stringsAsFactors = F)
View(swimming)
summary(swimming)
ss=swimming%>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(avgscore=mean(Score)) %>%
  arrange(desc(avgscore))
head(ss,20)
swimming1=filter(swimming, Country %in% c('United States', 'Australia','Greece',
                                          'China', 'Ireland','Japan', 'Ukraine',
                                          'Zimbabwe', 'Germany', 'Russia', 'Hungary',
                                          'Netherlands', 'Bulgaria', 'Canada',
                                          'Great Britain'))
#unique(df$Country)
swimming2=dcast(swimming1[,c('Country', 'Score', 'year')], year~ Country,value.var="Score")
#swimming3=dcast(swimming1[,c('Country', 'Percent', 'year')], year~ Country,value.var="Percent")
#swimming2[is.na(swimming2)]=0
color=c('red', 'pink', 'purple', 'indigo', 'blue', 
        'black', 'gray', 'teal', 'green', 'grey', 
        'lime', 'yellow', 'amber', 'orange', 'brown' )
dygraph(swimming2) %>%
  dyOptions(colors = color)

# dygraph(swimming3) %>%
#   dyOptions(colors = color)

athletics=read.csv('athletics.txt', header=T, stringsAsFactors = F)
ss1=athletics%>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(avgscore=mean(Score)) %>%
  arrange(desc(avgscore))
head(ss1, 20)
athletics1=filter(athletics, Country %in% c('United States','Russia', 'Kenya', 'Great Britain',
                                            'Germany', 'Finland', 'Jamaica', 'Ethiopia',
                                            'Poland', 'Sweden', 'Belarus','Cuba',
                                            'Australia', 'Romania', 'Greece'))
#athletics2[is.na(athletics2)]=0
athletics2=dcast(athletics1[,c('Country', 'Score', 'year')], year~ Country,value.var="Score")
dygraph(athletics2) %>%
  dyOptions(colors = color)

gymnastics=read.csv('gymnastics.txt', header=T, stringsAsFactors = F)
ss2=gymnastics%>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(avgscore=mean(Score)) %>%
  arrange(desc(avgscore))  
head(ss2,20)
gymnastics1=filter(gymnastics, Country %in% c('China', 'United States', 'Japan', 'Russia',
                                             'Romania', 'Switzerland', 'Germany', 'Italy',
                                             'Hungary', 'Austria', 'Greece', 'Belarus',
                                             'Ukraine', 'France', 'Finland'))
gymnastics2=dcast(gymnastics1[,c('Country', 'Score', 'year')], year~ Country,value.var="Score")
#gymnastics2[is.na(gymnastics2)]=0
dygraph(gymnastics2) %>%
  dyOptions(colors = color)




