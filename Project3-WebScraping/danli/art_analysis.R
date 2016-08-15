library(dplyr)
library(ggplot2)
library(tm)
library(SnowballC) 
library(wordcloud)

artist <- read.csv('artist_cleaned.csv', header = TRUE, stringsAsFactors = FALSE, na.strings=c("","NA"))
artist$century <- artist$born %/% 100

artist$firstName[artist$firstName  == "Sir"] <- NA
artist$firstName[artist$firstName  == "Master"] <- NA
artist$firstName[artist$firstName  == "The"] <- NA

# Some interesting ranks
century_rank <- arrange(as.data.frame(table(artist$century)), desc(Freq))
p = ggplot(century_rank, aes(x=reorder(century_rank$Var1, desc(century_rank$Freq)), y=century_rank$Freq)) + geom_bar(stat = 'identity', aes(fill= century_rank$Freq)) + xlab('Century') + ylab('Number of Artists') + ggtitle('Number of Artists by Century')
p$labels$fill <- "Number of Artists"
p

nation_rank <- arrange(as.data.frame(table(artist$nationality)), desc(Freq))
nation_rank_ <- head(nation_rank,20)
p1 = ggplot(nation_rank_, aes(x=reorder(nation_rank_$Var1, desc(nation_rank_$Freq)), y=nation_rank_$Freq)) + geom_bar(stat = 'identity', aes(fill= nation_rank_$Freq)) + xlab('Nation') + ylab('Number of Artists') + ggtitle('Number of Artists by Nationality') + theme(text = element_text(size=20)
p1$labels$fill <- "Number of Artists"
p1

genre_rank <- arrange(as.data.frame(table(artist$genre)), desc(Freq))
# firstName_rank <- arrange(as.data.frame(table(artist$firstName)), desc(Freq))
medium_rank <- arrange(as.data.frame(table(artist$medium)), desc(Freq))

# Wordcloud of artist name
nameCorpus <- Corpus(VectorSource(artist$firstName))
nameCorpus <- tm_map(nameCorpus, PlainTextDocument)
wordcloud(nameCorpus, max.words = 100, random.order = FALSE)

# Wordcloud of museum name
museumCorpus <- Corpus(VectorSource(artist$museums))
museumCorpus <- tm_map(museumCorpus, PlainTextDocument)
museumCorpus <- tm_map(museumCorpus, removePunctuation)
museumCorpus <- tm_map(museumCorpus, removeWords, stopwords('english'))
museumCorpus <- tm_map(museumCorpus, stemDocument)
wordcloud(museumCorpus, max.words = 200, random.order = FALSE)
