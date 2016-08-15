library(rjson)
library(dplyr)
library(ggplot2)
library(scales)

setwd('C:/Users/Charles/OneDrive/tedTalks')

talk_raw = read.csv('tedItems2.csv')


#Clean up data
clean_tab = talk_raw
clean_tab$rated = gsub("\n", "", talk_raw$rated)

#Create column to separate 1st rating, and 2nd rating
rlist = read.table(text = clean_tab$rated, sep = ",", colClasses = 'character')
clean_tab = cbind(talk_raw, rlist)

clean_tab$script = gsub('\'', "'", gsub("\n", " ", talk_raw$script))
clean_tab$title = gsub("\n", " ", talk_raw$title)
clean_tab$dist = gsub("\n", "", talk_raw$dist)

clean_tab$month = gsub("\n", "", talk_raw$month)
clean_tab$date = clean_tab$month
clean_tab$year = as.numeric(substr(clean_tab$month, 5, 9))
clean_tab$month = match(substr(clean_tab$month, 1, 3), month.abb)
clean_tab$views = as.numeric(gsub(",", "", talk_raw$views, fixed = TRUE))
clean_tab$date = as.Date(paste('1',clean_tab$date, sep = ' '), '%d %b %Y')

clean_tab$link = NULL
clean_tab$biolink = NULL
clean_tab$rated = NULL

##################

#basic numerical EDA

summary(clean_tab)
sapply(clean_tab, sd)

#The amount of male speakers to female speakers is 1416 to 337, an astounding amount.

ggplot(clean_tab, aes(x = date, y = views, color = sex, fill = sex)) + geom_smooth(alpha = 0.1)
#Women have a lot of views

ggplot(clean_tab, aes(x = views, color = sex, fill = sex)) + geom_density(alpha = 0.2)

#generally similar distributions - one woman went viral.

ggplot(clean_tab, aes(x = sex, fill = V1)) + geom_bar(position = 'fill', width = 0.5) + scale_fill_brewer(palette = 'Spectral') + coord_flip()

 
fcount = clean_tab %>% filter(sex == 'F') %>% count(V1)
mcount = clean_tab %>% filter(sex == 'M') %>% count(V1)
# 
fcount = cbind(fcount, Female = round(fcount$n/337*100, 1))
mcount = cbind(mcount, Male = round(mcount$n/1416*100, 1))

count_tab = cbind(fcount, mcount)
count_tab[4] = NULL

