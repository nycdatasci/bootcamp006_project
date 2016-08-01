#### UI #####
options(rgl.useNULL=TRUE)
library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)
library(car)
library(RColorBrewer)
library(reshape2)
library(shinyRGL)
library(stargazer)
library(raster)

var = c("bach_or_more", "veterans",  "percapita_income",  "hh_med_income", "fraction_votes", "over65", "white", "black", "hispanic", "asian", "female", "below_poverty")
names(var) = c("% Bachelors", "Total Veterans", "Per Capita Income", "Household Median Income", "Fraction of Votes", "% Over 65", "% White", " % Black", " % Hispanic", "% Asian", " % Female", "% Below Poverty")


shinyUI(navbarPage("2016 Primary Results",
                
                tabPanel("Introduction",
                         h1("Analysis of the 2016 Presidential Primaries"),
                         br(),
                         br(),
                         
                         
                         img(src = "http://image.syracuse.com/home/syr-media/width620/img/politics_impact/photo/20162550-mmmain.jpg"),
                         
                         br(),
                         
                         
                         br(),
                         h1("Table Of Contents"),
                         br(),
                         h4("1. Spatial Visualization of Bernie Sanders Primary Results"),
                         
                         br(),
                         h4("2. Spatial Visualization of Hillary Clinton Primary Results"),
                         br(),
                         h4("3. Spatial Visualization of Donald Trump Primary Results"),
                         br(),
                         h4("4. Spatial Visualization of Ted Cruz Primary Results"),
                         br(),
                         h4("5. Spatial Visualization of John Kasich Primary Results"),
                         br(),
                         h4("6. Regression Analysis"),
                         br(),
                         h4("7. 3D Visualization of How Education and Income Affected Voting Resutls for eac Candidate"),
                         br(),
                         br(),
                         
                         h4("Data Source"),
                         p("Source: ",a("Primary Results from CNN | Compiled by Ben Hammer",href=
                                          "https://www.kaggle.com/benhamner/2016-us-election")),
                         
                         p("Description: ","Demographic data, as well as vote results for each state")),
                
                tabPanel("Bernie Map",
                         tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                         leafletOutput("mapbern", height = 1400),
                         absolutePanel(top = 100, right = 20,
                                       selectInput("xcol", "Variables", var)
                         )
                ),
                
                
                tabPanel("Hillary Map",
                         tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                         leafletOutput("maphillary", height = 1400),
                         absolutePanel(top = 100, right = 20,
                                       #sidebarPanel(top = 1, right = 3,
                                       selectInput("xcol1", "Variables", var))),
                
                tabPanel("Donald Map",
                         tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                         leafletOutput("mapdonald", height = 1400),
                         absolutePanel(top = 100, right = 20,
                                       #sidebarPanel(top = 1, right = 3,
                                       selectInput("xcol2", "Variables", var))),
                
                tabPanel("Ted Map",
                         tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                         leafletOutput("mapted", height = 1400),
                         absolutePanel(top = 100, right = 20,
                                       #sidebarPanel(top = 1, right = 3,
                                       selectInput("xcol3", "Variables", var))),
                
                tabPanel("Kasich Map",
                         tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                         leafletOutput("mapkasich", height = 1400),
                         absolutePanel(top = 100, right = 20,
                                       #sidebarPanel(top = 1, right = 3,
                                       selectInput("xcol4", "Variables", var))),
                
                
                
                tabPanel("Regressions",
                         sidebarLayout(
                           sidebarPanel(top = 10, right = 2,
                                        selectInput("dataset2",label = "Select a dataset", choices = c("Donald"= "donald.merge", 
                                                                                                       "Bernie" = "bernie.merge",
                                                                                                       "Ted Cruz" = "tedcruz.merge",
                                                                                                       "Kasich" = "kasich.merge",
                                                                                                       "Hillary" = "hillary.merge"))),
                           mainPanel(
                             
                             tabsetPanel(
                               
                               tabPanel("Models",      
                                        helpText("The Response variable is:"),
                                        verbatimTextOutput("responseVar2"),
                                        br(),
                                        helpText("The current model is:"),
                                        
                                        verbatimTextOutput("modelEQ2")))))),
                
                
                
                tabPanel("3D Visualizer",                 
                         
                         
                         
                         p("When creating a model, it can be very helpful to visualize both the data and the model.
                           Often we wish to create a prediction model for a response variable on more than one predictors. 
                           In the case of a single response and two predictors, we must use a third dimension to visualize the 
                           the data and model."),
                         p("In this app, you will be able to  visualize the data and explore the effectiveness of different models
                           for a numerical response variable. "),
                         
                         sidebarLayout(
                           
                           
                           
                           
                           sidebarPanel(top = 10, right = 2, width = 2,
                                        
                                        selectInput("dataset",label = "Select a dataset", choices = c("Republican"= "rep", 
                                                                                                      "Democrats" = "dem",
                                                                                                      "Both Parties" = "repdem"
                                                                                                      
                                                                                                      
                                        )),
                                        
                                        ##To do the 2D this you need to uncomment the end and add the third option
                                        conditionalPanel(condition = "input.dataset == 'rep'",
                                                         radioButtons("expTypes1", label = "Fraction of Votes = ", 
                                                                      choices = list("Bachelor % + Median Income + Candidate" = 1), 
                                                                      
                                                                      selected = 1)),
                                        
                                        conditionalPanel(condition = "input.dataset == 'dem'",
                                                         radioButtons("expTypes2", label = "Fraction of Votes = ", 
                                                                      choices = list("Bachelor % + Median Income + Candidate" = 1), 
                                                                      selected = 1)),
                                        
                                        conditionalPanel(condition = "input.dataset == 'repdem'",
                                                         radioButtons("expTypes3", label = "Fraction of Votes = ", 
                                                                      choices = list("Bachelor % + Median Income + Candidate + Party" = 1), 
                                                                      
                                                                      selected = 1))
                                        
                                        
                                        
                           ),
                           mainPanel(webGLOutput("troisPlot",width="1100px",height="1100px"))
                           
                           
                           
                           
                         )
                         )#
                )
)
        
        
      