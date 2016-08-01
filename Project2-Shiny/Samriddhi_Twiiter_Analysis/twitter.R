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
library(Hmisc)


#------------------------------------------------------------------------------------------
connectTwitter = function(){
  
    consumer_key = 'bE7OZyqGwtVaJmowVEsUNdFmf'
    consumer_secret = 'UWjxpnscW3LooEUSKbgTaI1u3TAv13AZbZmsfuka6QQfzfi5pr'
    access_token = '701978984888868865-OZRSuWjJFJw5z53Z5HHU9p0RyPqVEY2'
    access_secret = '68XxLu15BkjQjBfAKYitpu21RQfgF1LIwKp86n2ALeE7I'
    setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)
  
}

connectTwitter()

#-----------------------------------------------------------------------------------------------

#The Twitter Streaming API, one of 3 such APIs (search, streaming, "firehose"), gives developers (and data scientists!) access to multiple types of streams (public, user, site), with the difference that the streaming API collects data in real-time (as opposed to the search API, which retrieves past tweets).

# SearchTweets = function(Searchvalue){
#       #connectTwitter()
#       tweets = searchTwitter(searchString = Searchvalue , n=100, lang='en',
#                               since = '2016-01-01
#                                      ', until= NULL ,geocode='42.08185,-78.43214,100mi')
      
SearchTweets = function(Searchvalue, NoOfTweets, FromDate, Todate){
        # connectTwitter()
        tweets = searchTwitter(searchString = Searchvalue , n=NoOfTweets, lang='en',
                               since = FromDate
                               , until= Todate ,geocode='42.08185,-78.43214,100mi',
                               retryOnRateLimit=1000)
      
                                     # until=NULL,locale=NULL,
                                     # sinceID=NULL, maxID=NULL,  resultType='recent',
                                     # retryOnRateLimit=120)
      #tweets
      #class(tweets) # list
      #converting list to vector
      # text=sapply(tweets, function(x) x$getText())
      # user=sapply(tweets, function(x) x$getScreenName())
      # RT=sapply(tweets, function(x) x$isRetweet)
      # latitude=sapply(tweets, function(x) as.numeric(x$latitude[1]))
      # longitude=sapply(tweets, function(x) as.numeric(x$longitude[1]))
      # time=sapply(tweets, function(x) format(x$created, format='%F %T'))
      # tweets_df = data.frame(text,user,RT,latitude,longitude,time)
      
      tweets_df <- twListToDF(tweets)
      return(tweets_df)
}

# Location is currently disabled by default in twitter; a user must choose to enable it for location to be available. That would constitute a clear decision to publish location. No sensible judiciary would regard it as an offence, of itself, to access an individual's location when the individual has themselves has chosen to publish it to a public place such as the web. 

#-----------------------------------------------------------------------------------------


#DT :: Datatable
DT_tweets = datatable(data = tweets_df , 
          rownames = FALSE, style = "default")
DT_tweets

#gives a dataframe
#arsenal_tweets.df = do.call("rbind",lapply(arsenal_tweets,as.data.frame))
# 
# iconv <- function(x, from = "", to = "", sub = NA, mark = TRUE)
# {
#   -    if(!is.character(x)) x <- as.character(x)
#   +    if(!is.character(x) && !is.raw(x)) x <- as.character(x)
#   .Internal(iconv(x, from, to, as.character(sub), mark))
# }

#-----------------------------------------------------------------------------------------


getTextData <- function(df) {
  # Gather corpus 
  textdata = Corpus(VectorSource(df$text))# creates corpus out of vector source
 # Corpora are collections of documents containing (natural language) text
  textdata =  textdata %>%
          tm_map(removeWords, stopwords("en"), mc.cores=1)  %>%
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


?iconv()

#-------------------------------------------------------------------------------------------

searchValue = "Arsenal"
search_tweet_df= SearchTweets(searchValue,100,'2016-01-01','2016-07-28')
textdata = getTextData(search_tweet_df)
#class(textdata)

#---------------------------------------------------------------------------------


#--------------------------------------------------------------------

# print most used words in wordcloud
pal = brewer.pal(9,"BuGn")
pal = pal[-(1:4)]
wordcloud::wordcloud(words = textdata,scale=c(4,.5),min.freq=5,
                     max.words=Inf, random.order=F, random.color=FALSE, rot.per=.1,
                     colors=pal,ordered.colors=FALSE,use.r.layout=FALSE,
                     fixed.asp=TRUE)

#-------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------




getSentiments <- function(df){
  #textdata = getTextData(search_tweet_df)
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
  # tot_sentiment = tot_sentiment %>%
  #   mutate(Total = ifelse( rownames(feelings) == c("anger"                                                     ,"anticipation",  "disgust","fear","sadness"),
  #                          count * (-1), count))
  Bar = gvisColumnChart(data= tot_sentiment,   xvar = "sentiment", yvar = "count")
  return (plot(Bar))
}

getSentiments(search_tweet_df)

#------------------------------------------------------------------------------------------

# maps of plotted tweets
search_tweet_df$loc=paste(search_tweet_df$latitude, search_tweet_df$longitude, sep=":")
M1 <- gvisMap(data = search_tweet_df, locationvar ="loc" , tipvar ="user",
              options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
                           mapType='hybrid', useMapTypeControl=TRUE,
                           width=800,height=400))

plot(M1) 



#--------------------------------------------------------------------------------------------



#tweets.df <- twListToDF(tweets2)
#------------------------------------------------------------------------------------------

# 10 to user with most tweets

Top10UserTweets <- function(df) {
  # Gather corpus 
  User_10_tweets = df %>% 
    group_by(screenName) %>% 
    summarise(TotalTweets= n()) %>% 
    arrange(desc(TotalTweets)) %>%
    head(10)
  
  Bar1 = gvisBarChart(User_10_tweets, xvar="screenName", yvar="TotalTweets", 
                      options=list(
                        chartArea="{left:150,top:50,width:'100%',height:'75%'}",
                        legend="bottom", 
                        title="Top 10 User with most tweets",
                        height = 600,
                        width = 700
                        
                        
                        ))
  return (plot(Bar1))
}

Top10UserTweets(search_tweet_df)


#-------------------------------------------------------------------------------------------

# Search Tweets of particular user 
# donald Trumph Tweets  @realDonaldTrump


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

HillaryTweets_df = tweetsByUser('HillaryClinton',3000)
TrumpTweets_df = tweetsByUser('realDonaldTrump',3000)

ggplot(trumpTweets_df)+geom_jitter(aes(x=wday,y=hour)) 
ggplot(HillaryTweets_df)+geom_jitter(aes(x=wday,y=hour)) 


getSentiments(HillaryTweets_df)
getSentiments(TrumpTweets_df)

#-----------------------------------------------------------------------------------------


tweetsByUserDate = function(username,numberTweets,FromDate, ToDate){
  
  #Tweets_df = tweetsByUser('realDonaldTrump',500)
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
tweetsByUserDate('realDonaldTrump',500,'2016-07-28','2016-07-29')

#---------------------------------------------------------------------------------------------
# top 5 Tweets
UserTop5Tweets = function(user, n){
  UserTweets_df = tweetsByUser(user,n) # ('realDonaldTrump',3000)
  UserTweetsTop5_df = UserTweets_df %>%
                    mutate (Tweetdate = format(as.Date(created,format="%Y-%m-%d"))) %>%
                    arrange(desc(retweetCount)) 
  UserTweetsTop5_df =  UserTweetsTop5_df[UserTweetsTop5_df$Tweetdate > "2015-12-31", ]
  UserTweetsTop5_df = head(UserTweetsTop5_df,5)
  return (UserTweetsTop5_df)
}

TrumpTop5 = UserTop5Tweets('realDonaldTrump',3000)
DT_TrumpTop5 = datatable(data = TrumpTop5 , rownames = FALSE, style = "default")
DT_TrumpTop5

HillaryTop5 = UserTop5Tweets('HillaryClinton',3000)
DT_HillaryTop5 = datatable(data = HillaryTop5 , rownames = FALSE, style = "default")
DT_HillaryTop5

#-----------------------------------------------------------------------------------------
# number of user tweets in 2016 calendar
Usertweets_calendar = function(user, n){
  UserTweets_df = tweetsByUser(user,n) # ('realDonaldTrump',3000)
  UserTweetsDate_df = UserTweets_df %>%
                    mutate (Tweetdate = format(as.Date(created,format="%Y-%m-%d"))) %>%
                    group_by(Tweetdate) %>% 
                    summarise(totalTweets = n())
  UserTweetsDate_df =  UserTweetsDate_df[UserTweetsDate_df$Tweetdate > "2015-12-31", ]
  UserTweetsDate_df$Tweetdate = as.Date(UserTweetsDate_df$Tweetdate)
  
  cal3 =gvisCalendar(UserTweetsDate_df, datevar = "Tweetdate", 
                     numvar = "totalTweets",
                     options=list(
                       title="Tweets in 2016",
                       calendar="{yearLabel: { fontName: 'Times-Roman',
                       fontSize: 32, color: '#1A8763', bold: true},
                       cellSize: 20,
                       cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                       focusedCellColor: {stroke:'red'}}",
                       height = 800,
                       width = 1200))
  return (plot(cal3))
}
Usertweets_calendar('realDonaldTrump',3000)
Usertweets_calendar('HillaryClinton',3000)

#--------------------------------------------------------------------------------------------

Usertweets_weekly = function(user, n){
  
  #------------------------------------------------------------------------
  UserTweets_df =   tweetsByUser('realDonaldTrump',1000)   # tweetsByUser(user,n)
  UserTweetsDate_df = UserTweets_df %>%
    mutate (Tweetdate = format(as.Date(created,format="%Y-%m-%d"))) %>%
    group_by(Tweetdate) %>% 
    summarise(totalTweets = n())
  UserTweetsDate_df =  UserTweetsDate_df[UserTweetsDate_df$Tweetdate > 
                                           (Sys.Date()-30), ]
  UserTweetsDate_df$Tweetdate = as.Date.date(UserTweetsDate_df$Tweetdate)
  UserTweetsDate_df
  
  TweetsTimeline = gvisAnnotationChart(UserTweetsDate_df, datevar="TweetDate",
                                       numvar="totalTweets", # idvar="text",
                                       titlevar="Title", annotationvar="Annotation",
                                       options=list(
                                         width=600, height=350))
       # Giving Errir  Error in as.Date.default(x) : 
  #   do not know how to convert 'x' to class "Date"                             )
  
  plot(TweetsTimeline)
  
#---------------------------------------------------------------
}


#---------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------

# tweets per day analysis of user

tweetsPerDay = function(df){
  Tweets_perDay = df %>% 
    group_by(wday) %>% 
    summarise(total = n()) %>%
    arrange(wday)
 # Tweets_perDay$wday = as.factor(Tweets_perDay$wday )
  
  Tweets_perDay =Tweets_perDay %>%
         mutate(Day = ifelse(wday == 1, "Sunday",
                      ifelse(wday == 2 , "Monday",
                      ifelse(wday == 3 , "Tuesday",
                      ifelse(wday == 4 , "Wednesday",
                      ifelse(wday == 5 , "Thursday",
                      ifelse(wday == 6, "Friday",
                      "Saturday" )))))))
  
  bar2 = gvisColumnChart(data = Tweets_perDay, xvar = "Day", yvar = "total",
                           options=list(
                           chartArea="{left:150,top:50,width:'100%',height:'75%'}",
                           legend="bottom", 
                           title=" Tweets per Day",
                           height = 600,
                           width = 700,
                           hAxis= "{  direction:1, 
                             slantedText:true, slantedTextAngle:90 }"
                         ))
  return (plot(bar2))
 
}
tweetsPerDay(HillaryTweets_df)
tweetsPerDay(trumpTweets_df)

#-----------------------------------------------------------------------------------------



ggplot(TrumpTweets_df,aes(x=wday))+geom_bar(aes(y = (..count..)),binwidth=1)

ggplot(TrumpTweets_df,aes(x=hour))+geom_bar(aes(y = (..count..)),binwidth=1)

ggplot(TrumpTweets_df)+geom_jitter(aes(x=wday,y=hour)) 

# as.POSIXlt -- Functions to manipulate objects of classes "POSIXlt" and "POSIXct" representing calendar dates and times.The as.POSIX* functions convert an object to one of the two classes used to represent date/times (calendar dates plus time to the nearest second). They can convert a wide variety of objects, including objects of the other class and of classes "Date", "date" (from package date),
#----------------------------------------------------------------------------------------


Tweets24Hour = function(searchValue, n){
  #searchValue = "#PokemonGo"
  search_tweet_df= SearchTweets(searchValue,n,as.character(Sys.Date()-1) ,
                                as.character( Sys.Date()))
  UserTweetsDate_df = search_tweet_df %>%
    # mutate (TweetHour = format(as.Date(created,format="%Y-%m-%d-%H"))) %>%
     mutate (TweetHour = format(created,format="%Y-%m-%d-%H")) %>%
     group_by(TweetHour) %>% 
     summarise(totalTweets = n()) 
   return (UserTweetsDate_df)
}
T_24_df = Tweets24Hour("Hillary",1000)

# cal3 =gvisCalendar(T_24_df, datevar = "created", numvar = "favoriteCount", options = 
#                list(chartArea="{left:150,top:50,width:'100%',height:'75%'}",
#                     legend="bottom",title=" Tweets in 24 hours",height = 600,
#                     width = 700))
# plot(cal3)
#------------------------------------------------------------------------------------
Tweets24 = T_24_df %>% 
            group_by(hour) %>% 
            summarise( TotalTweets = n())

Tweets24$hour = as.factor(Tweets24$hour )

bar3 = gvisScatterChart (data = Tweets24 #, xvar = "hour", yvar = "TotalTweets",
                       ,options=list(
                         chartArea="{left:150,top:50,width:'100%',height:'75%'}",
                         legend="bottom", 
                         title=" Tweets in 24 hours",
                         height = 600,
                         width = 700
                         #hAxis= "{  direction:1, 
                          #   slantedText:true, slantedTextAngle:90 }"
                       ))
plot(bar3)

#-------------------------------------------------------------------------------------------





#------------------------------------------------------------------------------------------

timeSeries = function(df, n){
  df$group = as.numeric(cut(df$created, breaks = n))
  df$group = as.factor(df$group)
  df_parts = c()
    for(i in 1:n)
    {
      df_1 = df[df$group == as.character(i), ]
      print (class(df_1))
      df_parts= c(df_parts, df_1)
    }
  return (df_parts)
}
DF_partition = timeSeries(search_tweet_df,3)
group_1 = as.data.frame(DF_partition[1:17])
group_2 = as.data.frame(DF_partition[18:34])
group_3 = as.data.frame(DF_partition[35:51])

t1 = getTextData(group_1)
t2 = getTextData(group_2)
t3 = getTextData(group_3)

pal = brewer.pal(9,"BuGn")
pal = pal[-(1:4)]
wordcloud::wordcloud(words = t1,scale=c(4,.5),min.freq=5,
                     max.words=Inf, random.order=F, random.color=FALSE, rot.per=.1,
                     colors=pal,ordered.colors=FALSE,use.r.layout=FALSE,
                     fixed.asp=TRUE)

wordcloud::wordcloud(words = t2,scale=c(4,.5),min.freq=5,
                     max.words=Inf, random.order=F, random.color=FALSE, rot.per=.1,
                     colors=pal,ordered.colors=FALSE,use.r.layout=FALSE,
                     fixed.asp=TRUE)

wordcloud::wordcloud(words = t3,scale=c(4,.5),min.freq=5,
                     max.words=Inf, random.order=F, random.color=FALSE, rot.per=.1,
                     colors=pal,ordered.colors=FALSE,use.r.layout=FALSE,
                     fixed.asp=TRUE)




#-------------------------------------------------------------------------------------------

?get_nrc_sentiment

install.packages('rsconnect')
rsconnect::setAccountInfo(name='samriddhishakya', token='4A76A95C4A2A879F0B70497A96BAAFFA', secret='7vFw3w4gATz+Z21qa4RScP94xyGAgzFzt267hBME')