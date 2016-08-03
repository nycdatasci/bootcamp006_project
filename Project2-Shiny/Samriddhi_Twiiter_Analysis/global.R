require(twitteR)
require(RCurl)
require(wordcloud) # text visual package
require(tm) # text mining package
require(RColorBrewer)
require(stringi)
require(stringr)
require(syuzhet)
require(googleVis)
require(ggplot2)
require(DT)
require(dplyr)
require(Hmisc)
require(shinythemes)
library(shiny)
library(shinyBS)
#install.packages("SnowballC")

# 10.0.0.16:3168

# -------------------------------twitter connection--------------------------------------------
connectTwitter = function(){
  consumer_key = 'bE7OZyqGwtVaJmowVEsUNdFmf'
  consumer_secret = 'UWjxpnscW3LooEUSKbgTaI1u3TAv13AZbZmsfuka6QQfzfi5pr'
  access_token = '701978984888868865-OZRSuWjJFJw5z53Z5HHU9p0RyPqVEY2'
  access_secret = '68XxLu15BkjQjBfAKYitpu21RQfgF1LIwKp86n2ALeE7I'
  setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)
}
#------------------------------------------------------------------------------------------------------

# '2016-01-01
SearchTweets = function(Searchvalue = 'Twitter', NoOfTweets, FromDate, Todate){
  tweets = searchTwitter(searchString = Searchvalue , n=NoOfTweets, lang='en',
                         since = FromDate
                         , until= Todate ,geocode='42.08185,-78.43214,100mi',
                         retryOnRateLimit=1000)
  tweets_df <- twListToDF(tweets)
  return(tweets_df)
}
#--------------------------------------------------------------------------------------------------------

getTextData <- function(df) {
  # Gather corpus 
  textdata = Corpus(VectorSource(df$text))# creates corpus out of vector source
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

#------------------------------------------------------------------------------------------------------

getSentiments <- function(df){
  textdata = getTextData(df)
  sentiments <- sapply(textdata, function(x) get_nrc_sentiment(as.character(x)))
  sentiments <- as.data.frame(aperm(sentiments)) # transpose and save as dataframe
  sentiments <- as.data.frame(lapply(sentiments, as.numeric)) # a bit more to organize
  sentiments <-
    sentiments %>%
    mutate(positivity = positive - negative)
  tweet_sentiments = lapply(sentiments,as.numeric)
  tweet_sentiments = as.data.frame(tweet_sentiments)
  tweet_sentiments = tweet_sentiments[,c(1,2,3,4,6,5,7,8,9,10)]
  feelings = data.frame("count"=colSums(tweet_sentiments))
  tot_sentiment <- cbind("sentiment" = rownames(feelings), feelings)
  return(tot_sentiment)
}
#------------------------------------------------------------------------------------------------------

Top10UserTweets <- function(df) {
  User_10_tweets = df %>% 
    group_by(screenName) %>% 
    summarise(TotalTweets= n()) %>% 
    arrange(desc(TotalTweets)) %>%
    head(10)
  return (User_10_tweets)
}

#------------------------------------------------------------------------------------------------------

tweetsByUser = function(user,numberTweets){
  Tweets = userTimeline(user,n=numberTweets,includeRts = FALSE,
                        excludeReplies=TRUE) # tweets from a user
  Tweets_df = twListToDF(Tweets)
  Tweets_df$month = sapply(Tweets_df$created,
                           function(x) {
                             p = as.POSIXlt(x);
                             p$mon})
  Tweets_df$hour= sapply(Tweets_df$created, 
                         function(x) {
                           p=as.POSIXlt(x);
                           p$hour})
  #label a tweet with a number corresponding to the day of the week
  Tweets_df$wday = sapply(Tweets_df$created, 
                          function(x) {
                            p=as.POSIXlt(x);
                            p$wday + 1})
  return (Tweets_df)
}

#----------------------------------------------------------------------------------------------------

tweetsByUserDate = function(username,numberTweets,FromDate, ToDate){
  Tweets = userTimeline(username,n=numberTweets,includeRts = FALSE,
                        excludeReplies=TRUE) # tweets from a user
  Tweets_df = twListToDF(Tweets)
  UserTweetsDate_df = Tweets_df %>%
    mutate (Tweetdate = format(as.Date(created,format="%Y-%m-%d")))
  
  UserTweetsDate_df =  UserTweetsDate_df[UserTweetsDate_df$Tweetdate >=  FromDate & 
                                           UserTweetsDate_df$Tweetdate <=  ToDate , ]
  UserTweetsDate_df$Tweetdate = as.Date(UserTweetsDate_df$Tweetdate)
  return (UserTweetsDate_df)
}
#tweetsByUserDate('realDonaldTrump',500,'2016-07-28','2016-07-29')

#------------------------------------------------------------------------------------------------
# number of user tweets in 2016 calendar
  
 Usertweets_calendar = function(user, numberTweets){
     UserTweets_df = tweetsByUser(user,numberTweets) # ('realDonaldTrump',3000)
     UserTweetsDate_df = UserTweets_df %>%
       mutate (Tweetdate = format(as.Date(created,format="%Y-%m-%d"))) %>%
       group_by(Tweetdate) %>% 
       summarise(totalTweets = n())
     UserTweetsDate_df =  UserTweetsDate_df[UserTweetsDate_df$Tweetdate > "2015-12-31", ]
     UserTweetsDate_df$Tweetdate = as.Date(UserTweetsDate_df$Tweetdate)
     return (UserTweetsDate_df)
 }
# Usertweets_calendar('realDonaldTrump',3000)

#---------------------------------------------------------------------------------------------------------
