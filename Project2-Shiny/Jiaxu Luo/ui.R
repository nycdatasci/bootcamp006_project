library(shiny)
library(DT)
library(shinydashboard)
library(bubbles)
library(d3treeR)


## UI side
shinyUI(dashboardPage(
        dashboardHeader(title = "Fortune 500 dashboard"),
        dashboardSidebar(sidebarUserPanel("NYC DSA",
                                          image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
                         sidebarMenu(
                                 menuItem("Map", tabName = "map", icon = icon("map")),
                                 menuItem("Treemap",tabName = "treemap",icon=icon("tree")),
                                 menuItem("Bubbles", tabName = "bubbles", icon = icon("circle")),
                                 menuItem("Data", tabName = "data", icon = icon("database"))
                         ),
                         
                         sliderInput("Year","Year of Interest",min=2006,max=2015,value=2006),
                         br(),
                         selectInput("Industry",
                                     "Industry of Interest",
                                     sort(choice_of_industry) )
        ),
        
        
        
        
        dashboardBody(
                tabItems(
                        tabItem(tabName = "map",
                                
                                fluidRow(
                                        box(htmlOutput("map"),width = "auto",status ="info",title = h4( "Number of Fortune 500 by industry") )
                                )),
                        tabItem(tabName = "treemap",
                                
                                fluidRow(
                                        box(d3tree2Output("treemap",height = "600px"),width = "auto",height="600")
                                )),
                        
                        tabItem(tabName = "bubbles",
                                
                                fluidRow(
                                        box(bubblesOutput("bubbles",width="600px"),height = 660,width = 7,title =h4( "Market.value(m) of companies")),
                                        box(htmlOutput("hist"),height = 660,width = 5,title=h4("P.e.ratio of companies"))
                                        
                                )),
                        
                        tabItem(tabName = "data",
                                fluidRow(box(DT::dataTableOutput("table"), width = 12)))
                )
        )
))
