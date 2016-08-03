
library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sidebarUserPanel("NYC DSA",
                     image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Analysis", tabName = "analysis", icon = icon("database"))
    )
    # selectizeInput("selected",
    #                "Select Item to Display",
    #                choice)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",

              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("minBox"),
                       infoBoxOutput("avgBox")),
              selectizeInput("selected",
                             "Select Item to Display",
                             choice),
              fluidRow(box(htmlOutput("map")),
                       box(htmlOutput("hist")))
              # fluidRow(box(selectInput("choice1", "Pick one feature to compare", choice)),
              #          box(selectInput("choice2", "Pick one feature to compare", choice)))
              ),
    tabItem(tabName = "analysis",
            fluidRow(box(selectInput("choice1", "Pick one feature to compare", choice)),
                     box(selectInput("choice2", "Pick one feature to compare", choice))),
            fluidRow(plotOutput("plot"))
    ),
      
      tabItem(tabName = "data",
              fluidRow(box(DT::dataTableOutput("table"), width = 15)))
    )
  )
))