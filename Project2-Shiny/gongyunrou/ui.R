library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    skin = "purple",
    dashboardHeader(title = "US Flights"),
    dashboardSidebar(
        sidebarUserPanel("Yunrou Gong",
                         image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Airport", tabName = "airport", icon = icon("plane")),
            menuItem("Motion Chart Yearly", tabName = "motion1", icon = icon("area-chart")),
            menuItem("Motion Chart Monthly", tabName = "motion2", icon = icon("area-chart")),
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
        tabItems(
            tabItem(tabName = "home",
                    h1("US Domestic Flights Visualization"),
                    fluidRow(
                      box(htmlOutput("home"), width=12,height = 300)
                      
                    )
              
            ),
            tabItem(tabName = "airport",
                    h2("Airport Location and Occuoancy level by Type and Year"),
                    fluidRow(
                      column(6,
                             sliderInput("Year_airport", "Year:",
                                         min = 1990, max = 2009, value =1990 ,step=1)
                        
                      ),
                      column(6,
                             selectInput(inputId = "airporttype",
                                         label = "Select Airport Type to Display:",
                                         choice <- c("Nonhub","SmallHub","MediumHub", "LargeHub"),
                                         selected = "LargeHub")
                        
                      )
                    ),
                    # sliderInput("Year_airport", "Year:",
                    #             min = 1990, max = 2009, value =1990 ,step=1),
                    # selectInput(inputId = "airporttype",
                    #             label = "Select Airport Type to Display:",
                    #             choice <- c("Nonhub","SmallHub","MediumHub", "LargeHub"),
                    #             selected = "LargeHub"),
                    
                    fluidRow(
                      box(htmlOutput("airport"), width=12))
              
            ),
            tabItem(tabName = "motion1",
                    h2("Motion Chart Yearly from 1990 to 2009"),
                    fluidRow(
                      box(htmlOutput("motion1"), width=12,height = 300))
            
                    
            ),
            tabItem(tabName = "motion2",
                    h2("Motion Chart Monthly"),
                    fluidRow(
                      sliderInput("Year_motion2", "Year:",
                                  min = 1990, max = 2009, value =1990),
                      box(htmlOutput("motion2"), width=12,height = 300))

                    
              
            ),
            
            tabItem(tabName = "map",
                    h2("Netflow of Passengers by State"),
                    # fluidRow(infoBoxOutput("maxBox"),
                    #          infoBoxOutput("minBox"),
                    #          infoBoxOutput("avgBox")),
                    fluidRow(box(htmlOutput("map"), height = 300),
                             # box(htmlOutput("hist"), height = 300))),
                    sliderInput("Year_map", "Year:",
                                min = 1990, max = 2009, value =1990,step=1),
                    sliderInput("Month_map", "Month:",
                                min = 1, max = 12, value =1,step=1))),
            
            tabItem(tabName = "data",
                    fluidRow(box(DT::dataTableOutput("table"), width = 12)))
        )
    )
))