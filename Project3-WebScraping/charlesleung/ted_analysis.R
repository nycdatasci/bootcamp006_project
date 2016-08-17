library(rjson)
library(dplyr)
library(ggplot2)
library(scales)

#Load data
setwd('C:/Users/Charles/OneDrive/tedTalks')
talk_raw = read.csv('tedItems3.csv')

#Clean up data
clean_tab = talk_raw
clean_tab$rated = gsub("\n", "", talk_raw$rated)

#Create column to separate 1st rating, and 2nd rating
rlist = read.table(text = clean_tab$rated, sep = ",", colClasses = 'character')
clean_tab = cbind(talk_raw, rlist)

clean_tab$script = gsub('\'', "'", gsub("\n", " ", talk_raw$script))
clean_tab$script = gsub(pattern = '\"', "", clean_tab$script)
clean_tab$title = gsub("\n", " ", talk_raw$title)
clean_tab$dist = gsub("\n", "", talk_raw$dist)
clean_tab$V2 = gsub(' ','',clean_tab$V2)
clean_tab$month = gsub("\n", "", talk_raw$month)
clean_tab$date = clean_tab$month
clean_tab$year = as.numeric(substr(clean_tab$month, 5, 9))
clean_tab$month = match(substr(clean_tab$month, 1, 3), month.abb)
clean_tab$views = as.numeric(gsub(",", "", talk_raw$views, fixed = TRUE))
clean_tab$date = as.Date(paste('1',clean_tab$date, sep = ' '), '%d %b %Y')
clean_tab$min = round(as.numeric(substr(as.character(clean_tab$duration),0,2)) + as.numeric(substr(as.character(clean_tab$duration), 4, 5))/60, 2)

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

# VIEW VS GENDER DENSITY PLOT
ggplot(clean_tab, aes(x = views, color = sex, fill = sex)) + geom_density(alpha = 0.2)
#generally similar distributions - one woman went viral.

adj_tab1 = clean_tab[8:9]
adj_tab2 = clean_tab[c(8,10)]
names(adj_tab2) = c('sex', 'V1')
adj_tab = rbind(adj_tab1, adj_tab2)
# GENDER VS ADJ, STACKED BAR
ggplot(adj_tab, aes(x = sex, fill = V1)) + geom_bar(position = 'fill', width = 0.5) + scale_fill_brewer(palette = 'Spectral') + coord_flip()
#ggplot(clean_tab, aes(x = sex, fill = V2)) + geom_bar(position = 'fill', width = 0.5) + scale_fill_brewer(palette = 'Spectral') + coord_flip()
#Men more likely to be Ingenious, 

adj_count = adj_tab %>% group_by(sex) %>% count(V1)
mvec = (adj_count %>% filter(sex == 'M') %>% select(n))[2]
fvec = (adj_count %>% filter(sex == 'F') %>% select(n))[2]
chisq.test(x = cbind(mvec, fvec))

#p value is very low, so there must be some dependence between the variables.

#  
#fcount = clean_tab %>% filter(sex == 'F') %>% count(V1, V2)
#mcount = clean_tab %>% filter(sex == 'M') %>% count(V1, V2)

#fcount$totals = fcount$V1 + fcount$V2
#mcount$totals = mcount$V1 + mcount$V2

#chisq.test(x = fcount$, y = mcount)
# # 
# fcount = cbind(fcount, Female = round(fcount$n/337*100, 1))
# mcount = cbind(mcount, Male = round(mcount$n/1416*100, 1))
# 
# count_tab = cbind(fcount, mcount)
# count_tab[4] = NULL

# DURATION vs VIEW
ggplot(clean_tab, aes(x = min, y = views)) + geom_point(alpha = 0.1, color = 'red')
durmodel = lm(views ~ min, data = clean_tab) #initial plot looks like it can be linear
summary(durmodel) #p value is 0.08, so we reject null hypothesis. 
par(mfrow = c(2,2))
plot(durmodel) #very not normal Q-Q, extremely heavy tails towards the end. 

# check all variables
lin.s = lm(views ~ min + sex + date, data = clean_tab) #initial plot looks like it can be linear
summary(lin.s) #date and min seem significant
par(mfrow = c(2,2))
plot(lin.s) #Q-Q looks really bad. very heavy tail on higher quartile. residuals vs fitted shows no pattern.
#residuals vs leverage shows many points beyond allowable cooks distance - aka many outliers.

par(mfrow = c(1,1))
library(car)
bc = boxCox(lin.s)
lambda = bc$x[which(bc$y == max(bc$y))] 
views.bc = (clean_tab$views^lambda - 1)/lambda

bc.s = lm(views.bc ~ min + sex + date, data = clean_tab)

summary(bc.s)
#date is very significant, though minute is also kind of significant. Both have an effect of e-06 or e-04 on log scale.
par(mfrow = c(2,2))
plot(bc.s) #Q-Q looks much better

par(mfrow = c(1,1))
plot(x = clean_tab$date, y = views.bc)
plot(x = clean_tab$min, y = views.bc)
# lambda = -2/9, so must unboxtransform to understand. (*(-2/9) + 1)^(2/9)

# TEXT ANALYSIS
library(tm)
library(qdapDictionaries)

#word count on each
wordCount = function(x){sapply(gregexpr("\\W+", x), length) + 1}
clean_tab$words = lapply(clean_tab$script, wordCount)

for (i in 1:1752){
  clean_tab$rate[i] = clean_tab$words[[i]] / clean_tab$min[i]
}

## Difference between WPM for men and women? views?
ggplot(clean_tab, aes(x = min, fill = sex, color = sex)) + geom_density(alpha = 0.2)
ggplot(clean_tab, aes(x = rate, fill = sex, color = sex)) + geom_density(alpha = 0.2)

script.t = removeWords(stripWhitespace(removeWords(removePunctuation(removeNumbers(tolower(clean_tab$script))), stopwords('english'))), c('laughter', 'applause'))

textAn = as.data.frame(cbind(clean_tab$V1, clean_tab$V2, clean_tab$views, script.t))
textAn$V3 = as.numeric(textAn$V3)

# tdmMatrix = function(x){
#   return(TermDocumentMatrix(Corpus(VectorSource(x))))
# }
# 
# script.t = lapply(script.t, tdmMatrix)
#Now I have a list of TDM documents, I can conduct analyses with word frequences.
library(wordcloud)

#Overall WC
OvCorp = TermDocumentMatrix(Corpus(VectorSource(textAn[4])))
OvFreq = rowSums(as.matrix(OvCorp))
set.seed(123)
wordcloud(names(OvFreq), OvFreq, max.words = 250, colors = brewer.pal(8, "Paired"))


#Inspiring WC
Inspmat = textAn %>% filter((V1 == 'Inspiring') | (V2 == 'Inspiring'))
InspCorp = TermDocumentMatrix(Corpus(VectorSource(Inspmat[4])))
InspFreq = rowSums(as.matrix(InspCorp))
set.seed(123)
wordcloud(names(InspFreq), InspFreq, max.words = 250, colors = brewer.pal(8, "RdGy"))

#Informative WC
Infomat = textAn %>% filter((V1 == 'Informative') | (V2 == 'Informative'))
InfoCorp = TermDocumentMatrix(Corpus(VectorSource(Infomat[4])))
InfoFreq = rowSums(as.matrix(InfoCorp))
set.seed(123)
wordcloud(names(InfoFreq), InfoFreq, max.words = 250, colors = brewer.pal(8, "PuOr"))

#Fascinating WC
Fascmat = textAn %>% filter((V1 == 'Fascinating') | (V2 == 'Fascinating'))
FascCorp = TermDocumentMatrix(Corpus(VectorSource(Fascmat[4])))
FascFreq = rowSums(as.matrix(FascCorp))
set.seed(123)
wordcloud(names(FascFreq), FascFreq, max.words = 250, colors = brewer.pal(8, "PRGn"))

#Beautiful WC
Beamat = textAn %>% filter((V1 == 'Beautiful') | (V2 == 'Beautiful'))
BeaCorp = TermDocumentMatrix(Corpus(VectorSource(Beamat[4])))
BeaFreq = rowSums(as.matrix(BeaCorp))
set.seed(123)
wordcloud(names(BeaFreq), BeaFreq, max.words = 250, colors = brewer.pal(8, "PiYG"))

#Courageous
Courmat = textAn %>% filter((V1 == 'Courageous') | (V2 == 'Courageous'))
CourCorp = TermDocumentMatrix(Corpus(VectorSource(Courmat[4])))
CourFreq = rowSums(as.matrix(CourCorp))
set.seed(123)
wordcloud(names(CourFreq), CourFreq, max.words = 250, colors = brewer.pal(8, "RdBu"))

#Funny
Funmat = textAn %>% filter((V1 == 'Funny') | (V2 == 'Funny'))
FunCorp = TermDocumentMatrix(Corpus(VectorSource(Funmat[4])))
FunFreq = rowSums(as.matrix(FunCorp))
set.seed(123)
wordcloud(names(FunFreq), CourFreq, max.words = 250, colors = brewer.pal(8, "Purples"))

#Persuasive
Persmat = textAn %>% filter((V1 == 'Persuasive') | (V2 == 'Persuasive'))
PersCorp = TermDocumentMatrix(Corpus(VectorSource(Persmat[4])))
PersFreq = rowSums(as.matrix(PersCorp))
set.seed(123)
wordcloud(names(PersFreq), PersFreq, max.words = 250, colors = brewer.pal(8, "YlGn"))

#Ingenious

Ingmat = textAn %>% filter((V1 == 'Ingenious') | (V2 == 'Ingenious'))
IngCorp = TermDocumentMatrix(Corpus(VectorSource(Ingmat[4])))
IngFreq = rowSums(as.matrix(IngCorp))
set.seed(123)
wordcloud(names(IngFreq), IngFreq, max.words = 250, colors = brewer.pal(8, "Spectral"))

#WC with median Views = 1044469

Lowmat = textAn %>% filter(V3 <= 1044469)
LowCorp = TermDocumentMatrix(Corpus(VectorSource(Lowmat[4])))
LowFreq = rowSums(as.matrix(LowCorp))
set.seed(123)
wordcloud(names(LowFreq), LowFreq, max.words = 250, colors = brewer.pal(8, "Greys"))

#WC high

Highmat = textAn %>% filter(V3 > 1044469)
HighCorp = TermDocumentMatrix(Corpus(VectorSource(Highmat[4])))
HighFreq = rowSums(as.matrix(HighCorp))
set.seed(123)
wordcloud(names(HighFreq), HighFreq, max.words = 250, colors = brewer.pal(8, "Dark2"))