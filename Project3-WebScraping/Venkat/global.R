#Global
library(ggmap)
library(googleVis)
library(ggplot2)
library(wordcloud)
library(tm)
library(wordcloud2)
library(leaflet)
library(SnowballC)
library(stringr)
library(stringi)

df1 <- read.csv('df1.csv', stringsAsFactors = F)

output1 <- read.csv('output_sentiment.csv', stringsAsFactors = F)

output2 <- read.csv('daa.csv', stringsAsFactors = F, header = FALSE)
names(output2)[1] <- "NAME"
names(output2)[2] <- "REVIEW"

data = merge(df1, output1, by = "NAME")

name = data$NAME
NAME = output2$NAME


getTextData = function(df) {
  # Gather corpus 
  textdata = Corpus(VectorSource(df$REVIEW))# creates corpus out of vector source
  # Corpora are collections of documents containing (natural language) text
  textdata =  textdata %>%
    tm_map(removeWords, stopwords("english"))  %>%
    tm_map(removeWords,  c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", 
                           "but","will","say","like","and","for","just"))  %>%
    tm_map(removePunctuation, mc.cores=1) %>%
    tm_map(content_transformer( function(x) 
      if(Encoding(x)=='unknown'){
        iconv(x, from='ASCII',to='UTF-8', sub='byte')
      })) %>%    
    tm_map(content_transformer(stri_trans_tolower),mc.cores=1) %>%
    tm_map(content_transformer(function(x) str_replace_all(x, "@\\w+", "")), 
           mc.cores=1) %>% # remove twitter handles
    tm_map(removeNumbers, mc.cores=1) %>%
    tm_map(stemDocument, mc.cores=1) %>% # Stem words in a text document
    #using Porter's stemming algorithm.
    tm_map(stripWhitespace, mc.cores=1)
  return (textdata)
}