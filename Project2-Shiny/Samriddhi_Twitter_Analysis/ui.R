require(shinydashboard)
require(shiny)
require(googleVis)
require(DT)
require(twitteR)
require(RCurl)
require(wordcloud) # text visual package
require(tm) # text mining package
require(RColorBrewer)
require(stringi)
require(stringr)
require(syuzhet)
require(ggplot2)
require(dplyr)
require(Hmisc)
require(shinythemes)
require(shinyBS)

shinyUI(
  navbarPage(
    theme = "CSS/bootstrap3.css",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "CSS/style.css")
    ),
    

    tabPanel(
      
      "Introduction",
      
      fluidRow(column(
        9,
        offset = 2,
        img(
          src = 'twitter.jpg',
          height = 100,
          width = 860,
          align = "middle"
        )
      )),
      br(),
      fluidRow(column(
        9, offset = 2,
        img(
          src = 't_h.jpg',
          height = 400,
          width = 860
        )
      )),
      fluidRow(column(
        8,
        offset = 2,
        # h3('Twitter Analysis about Presidential Candidates?'),
        br(),
        
        p(
          "The 2016 November presidential election is fast approaching and President Barack Obama's second term is about to come to an end. Both Republican and Democratic parties have nominated their candidate for president which sees Donald Trump battling Hillary Clinton for the seat in White House. The public sentiment towards the candidates plays an important role in influencing the future leader of United States. For this Shiny project, I am interested in finding out public sentiments towards the presidential candidates Donald Trump and Hillary Clinton. The feelings towards candidates fluctuate quickly as interviews, debates, responses to global events, and other issues come to front.",
          style = "font-family: 'Comic Sans MS';  text-align:justify"
        ),
        br(),
        
        p(
          "Twitter provides us with a good platform to access the opinions about the Presidential Candidates from people. We can get real time tweets about the candidates and know if it is a positive or negative. This application focuses on tweets made by people towards Presidential candidate and also the tweets made by Presidential Candidates themselves. It analyzes the tweets and predict the public sentiments towards each candidate. The sentiments have been classified into 8 categories which were later combined into positive or negative response.  The eight categories are anger, anticipation, disgust, fear, sadness, joy, surprise and trust. This application also displays frequently used words in the tweets. " ,
          style = "font-family: 'Comic Sans MS';  text-align:justify"
        ),
        br(),
        
        p(
          "This application also takes a closer look at the tweets made by the candidates and sees what kind of sentiments and words were found in their tweets. It also tells how many tweets were posted for each date in 2016 calendar by the candidates. The results for all the analysis depend on number of tweets searched in the application and dates provided. This application attempts to understand the current sentiment towards presidential candidates based off the public's live opinions. ",
          style = "font-family: 'Comic Sans MS';  text-align:justify"
        ),
        br(),br(),br()
      ))
    ),
    
    tabPanel(
      "Tweets by Candidates",
      sidebarLayout(
        sidebarPanel(
          
          img(
            src = 'Twitter-logo21.png',
            height = 50,
            width = 50
          ),
          
          br(),br(),
          selectInput(
            "candidate1",
            "Select Candidate:",
            c("Hillary Clinton" = "HillaryClinton" ,
              "Donald Trump " = "realDonaldTrump"),
            selected = "realDonaldTrump",
            selectize = FALSE
          ),
          
          sliderInput(
            "NoOfTweets1",
            #
            label = "Number of tweets:",
            min = 0,
            max = 3000,
            value =  100
          ),
          
          dateRangeInput(
            "dateRange1",
            "Date range",
            start = "2016-01-01",
            end = as.character(Sys.Date())
          ),
          
          actionButton(
            inputId = "SearchButton1",
            label = "Search",
            width = 100
          ),
          
          
          br(),
          br(),
          br(),
          br(),
          actionButton("tabWC1", "Word Cloud"),
          
          actionButton("tabCal1", "Tweet Calendar"),
          
          actionButton("tabSentiment1", "Sentiments")
          
          #
          # checkboxGroupInput('show_vars', 'Columns in diamonds to show:',
          #                     names (tweetsByUser_DF2),  # field names
          #                     selected = names(tweetsByUser_DF2) )
          
          # to display  in UI the columns of dataframe from tweets
          #  uiOutput("show_vars")
          
        ),
        
        mainPanel(
          DT::dataTableOutput("table1",  height = 500),
          
          bsModal(
            "modalWordCloud1",
            "Word Cloud",
            "tabWC1",
            size = "large",
            plotOutput("wordcloud1")
          ),
          
          bsModal(
            "modalCalendar1",
            "Tweets By Calendar Year 2016",
            "tabCal1",
            size = "large",
            htmlOutput("Calendar1")
          ),
          
          bsModal(
            "modalSentiments1",
            "Sentimental Anaysis of Tweets",
            "tabSentiment1",
            size = "large",
            htmlOutput("Sentiments1")
          )
          
        )
      )
    ),
    
    
    #  navbarMenu("Tweets by People",
    tabPanel(
      "Tweets about Candidates",
      sidebarLayout(
        sidebarPanel(
          img(
            src = 'Twitter-logo21.png',
            height = 50,
            width = 50
          ),
          br(),br(),
          
          selectInput(
            "Candidates2",
            "Select Candidate:",
            c("Trump" , "Hillary"),
            selected = "Trump",
            selectize = FALSE
          ),
          
          dateRangeInput(
            "dateRange2",
            "Date range",
            start = "2016-01-01",
            end = as.character(Sys.Date())
          ),
          
          sliderInput(
            "NoOfTweets2",
            #
            label = "Number of tweets:",
            min = 0,
            max = 3000,
            value =  100
          ),
          
          actionButton(
            inputId = "SearchButton2",
            label = "Search",
            width = 100
          ),
          
          br(),
          br(),
          br(),
          br(),
          actionButton("tabWC2", "Word Cloud"),
          actionButton("tabTop5User2", "Who Tweeted Most"),
          actionButton("tabSentiment2", "Sentiments")
          
        ),
        mainPanel(
          DT::dataTableOutput("table2",  height = 500),
          
          bsModal(
            "modalWordCloud2",
            "Word Cloud",
            "tabWC2",
            size = "large",
            plotOutput("wordcloud2")
          ),
          
          bsModal(
            "modalCalendar2",
            "Users who tweeted most about the candidates",
            "tabTop5User2",
            size = "large",
            htmlOutput("plot2")
          ),
          
          bsModal(
            "modalSentiments2",
            "Sentimental Anaysis of Tweets",
            "tabSentiment2",
            size = "large",
            htmlOutput("Sentiments2")
          )
        )
      )
    )
  )
  
  # ) #close navebarpage
) #close shinyui