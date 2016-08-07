install.packages('shinythemes')
library(shinythemes)

tabPanelAbout <- source("source/intro.R", local=T)$value



shinyUI(fluidPage(theme=shinytheme("flatly"), 
                  navbarPage(title=" ",
                    tabPanel("Welcome", value="welcome"),
                    tabPanel("Getting Started", value="gettingstarted",icon = icon('play')),
                    tabPanel("Face Match", value="facematch",icon = icon('users')),
                    tabPanel("Feature Analysis",value="featureanalysis",icon = icon('area-chart')),
                    tabPanel("Image Analysis", value="imageanalysis",icon = icon('dashboard')),
                    tabPanel("Find Faces", value="findface",icon = icon('search-plus')),
                    
                    navbarMenu("More",
                               tabPanel("Help",icon = icon('question')),
                               tabPanel("Contact us",icon = icon('envelope'))
                    ),
                   
                    windowTitle="Facial Analysis",
                    collapsible=TRUE,
                    id="tsp"
                  ),
              
                  #tags$head(includeScript("ga-random_forest_example.js"), includeScript("ga-allapps.js")),
                  #tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
                  #fluidRow(column(12, "I would use dropdown menus to condense the navbar above but 'tabsetPanel' cannot take an 'id' argument in the current version of Shiny.")),
               
                  #source("source/sidepage.R",local=T)$value,source("source/mainpage.R",local=T)$value
                  
                  fluidRow(
                    source("source/sidepage.R",local=T)$value,source("source/mainpage.R",local=T)$value
                
                  )
                 
))