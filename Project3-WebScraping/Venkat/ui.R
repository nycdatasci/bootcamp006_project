#UI


library(shiny)
library(ggplot2)
library(dygraphs)
library(stringr)
library(scales)
library(leaflet)
library(SnowballC)
library(tm)
library(stringi)

shinyUI(
  navbarPage(
    title = "EDA And Sentiment Analysis Of Yelp Data",
    #theme = shinytheme("flatly"),
    
   
    
    tabPanel("Top Restaurant Around Times Square",
             sidebarLayout(
               sidebarPanel(
                 helpText("Top restaurants based on Yelp reviews")
                 
                 
               ),
               mainPanel(style="position:relative",
                         h2("Resaurants around times square "),
                         br(),
                         leafletOutput("plot_rm", width = "100%")
                         #htmlOutput("plot_rm", width = "100%")
               )
             )
    ),
    
    tabPanel("Distribution",
             sidebarLayout(
               sidebarPanel(
                 helpText("Distribution of Restaurant Details for the first 20 pages")
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Distribution Of Rating", plotOutput("plot_tr")),
                   tabPanel("Distribution Of Number of Reviews", plotOutput("plot_rd"))
                 )
               )
             )
             
    ),
    
    tabPanel("World Cloud",
             sidebarLayout(
               sidebarPanel(
                 helpText("Word Cloud"),
                 selectInput("select1", label = h3("Select box"),
                             choices = NAME,
                             selected = NAME[1])
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data", plotOutput("plot_wc"))
                 )
               )
             )
    ),
    
    
    
    tabPanel("Sentiment Analysis",
             sidebarLayout(
               sidebarPanel(
                 helpText("Sentiment Analysis of Yelp User Reviews"),
                 selectInput("select2", label = h3("Select box"),
                             choices = name,
                             selected = name[1])

               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data", htmlOutput("plot_ta")),
                   tabPanel("Sentiment Analysis", plotOutput("plot_tt"))
                 )
               )
             )
    )

  )
  
)