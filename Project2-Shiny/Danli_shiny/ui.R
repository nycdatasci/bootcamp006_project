library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

shinyUI(dashboardPage(
  skin = "yellow",
  ######### Dashboard Header ###############
  dashboardHeader(title = "Global Entrepreneurship Monitor 2015",
                  titleWidth = 380),
  
  ######### Dashboard Sidebar ##############
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Global Overview", tabName = "map", icon = icon("map")),
      selectInput("Ratio",
                  "Choose your Ratio to Map",
                  choice = list(
                    "Perceived Opportunities",
                    "Perceived Capabilities",
                    "Fear of Failure Rate",
                    "Entrepreneurial Intention"
                  ), selected = "Perceived Opportunities"),
      
      menuItem("Country Profile", tabName = "radar", icon = icon("map-marker")),
      selectizeInput("Country",
                     "Choose your Countries to Compare",
                     choice = nes$Economy, selected = "USA",
                     multiple = TRUE),
      
      menuItem("Country Rank", tabName = "rank", icon = icon("list-ol")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("About", tabName = "info", icon = icon("info"))
    )
  ),
  
  ######### Dashboard Body #################
  dashboardBody(
    tabItems(
      
      # ---- Display Global Map in this tab ----
      tabItem(tabName = "map",
              tags$style(type = "text/css", "#mymap {height: calc(100vh - 120px) !important;}"),
              fluidRow(box(title = textOutput("mapname"),
                           width = 12, solidHeader = TRUE,
                           leafletOutput("mymap")))
      ),
      tabItem(tabName = "radar",
              fluidRow(box(title = uiOutput("radarname"),
                           width = 12, solidHeader = TRUE,
                           plotOutput("myradar", width = "100%", height = "650px"))
              )
      ),
      
      tabItem(tabName = "rank", 
              fluidRow(
                box(
                  title = "Entrepreneurship Ranks", status = "warning",
                  sliderInput("rank_", 
                              "Choose Number of Countries to Rank", min = 0, max = 60, value = 6)
                )
                ,
                box(
                  title = "Entrepreneurship Ranks", 
                  selectInput("Ratio2",
                              "Choose 2 Ratios to Rank",
                              choice = names(aps)[-c(1,2)],
                              selected = c("Perceived.Opportunities", "Perceived.Capabilities"),
                              multiple = TRUE)
                )
              ),
              
              fluidRow(box(width = 6, solidHeader = TRUE,
                           plotOutput("Rank1")),
                       box(width = 6, solidHeader = TRUE,
                           plotOutput("Rank2"))
              )       
      ),
      
      tabItem(tabName = "table",
              fluidRow(
                tabBox(
                  title = "Data Tables",
                  id = "tables", height = "650px", width = "100%",
                  tabPanel("National Expert Survey",
                           DT::dataTableOutput("nesTable", width = "100%")),
                  tabPanel("Adult Population Survey",
                           DT::dataTableOutput("apsTable", width = "100%"))
                )
              )
              ),
      
      tabItem(tabName = "info", 
              h4("Data Source"),
              p("Source: ",a("Global Entrepreneurship Monitor",href="http://www.gemconsortium.org/data")),
              p("Description: ","The Global Entrepreneurship Monitor is the world's foremost study of entrepreneurship. They collaborate with organizations across the globe conducting annual entrepreneurship surveys."),
              p("Usage: ","NES(National Expert Survey)2015 contains information about the national context and how that impacts entrepreneurship. APS(Adult Population Survey)2015 contains information about the entrepreneurial behaviour and attitudes of individuals. Both files are downloaded on July 22nd."),
              p("Project Code: ",a("git@github.com:danlizeng/Shiny_Project.git", href = "https://github.com/danlizeng/Shiny_Project.git")),
              br(),
              h4("Author Information"),
              p("Danli Zeng"),
              p("Email: danli@nyu.edu"),
              p("LinkedIn:", a("https://www.linkedin.com/in/danli-zeng-39856095",href="https://www.linkedin.com/in/danli-zeng-39856095")),
              br(),
              br()
      )
    )
  )
)
)
