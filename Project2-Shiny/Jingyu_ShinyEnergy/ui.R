library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "My Dashboard"),
    dashboardSidebar(
        sidebarUserPanel("NYC DSA",
                         image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
        sidebarMenu(
            menuItem("Plot", tabName = "plot", icon = icon("line-chart")),
            menuItem("Map", tabName = "map", icon = icon("map"))
        ),
        selectizeInput("selected",
                       "Select Source to Display",
                       choice, selected='Total Generation', multiple=TRUE),
        #sidebarUserPanel("NYC DSA",
         #                image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
        br(),
        sidebarMenu(
          menuItem("Plot1", tabName = "plot1", icon = icon("line-chart")),
          menuItem("Map1", tabName = "map1", icon = icon("map"))
        ),
        selectizeInput("selected1",
                       "Select Renewable Source to Display",
                       choice1,selected='Total Generation', multiple=TRUE),
        br(),
        sidebarMenu(
          menuItem("Plot2", tabName = "plot2", icon = icon("line-chart"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "plot",
                  #  fluidRow(infoBoxOutput("maxBox"),
                  #           infoBoxOutput("minBox"),
                  #           infoBoxOutput("avgBox")),
                     fluidRow(box(htmlOutput("plot_1"), height = 350, width = 800),
                              box(htmlOutput("plot_2"), height = 350, width = 800))
                    ),
            #####
            tabItem(tabName = "map",
                    #  fluidRow(infoBoxOutput("maxBox"),
                    #           infoBoxOutput("minBox"),
                    #           infoBoxOutput("avgBox")),
                    fluidRow(#box(htmlOutput("plot_1"), height = 350, width = 800),
                             box(htmlOutput("plot_3"), height = 350, width = 800))
            ),
            #######
          #  tabItem(tabName = "data",
          #          fluidRow(box(DT::dataTableOutput("table"), width = 12))),
           
             tabItem(tabName = "plot1",
                    #  fluidRow(infoBoxOutput("maxBox"),
                    #           infoBoxOutput("minBox"),
                    #           infoBoxOutput("avgBox")),
                    fluidRow(box(htmlOutput("plot_4"), height = 350, width = 800),
                             box(htmlOutput("plot_5"), height = 350, width = 800))
            ),
            #####
            tabItem(tabName = "map1",
                    #  fluidRow(infoBoxOutput("maxBox"),
                    #           infoBoxOutput("minBox"),
                    #           infoBoxOutput("avgBox")),
                    fluidRow(#box(htmlOutput("plot_1"), height = 350, width = 800),
                             box(htmlOutput("plot_6"), height = 350, width = 800))
            ),
            #######
            tabItem(tabName = "plot2",
                    fluidRow(box(htmlOutput("plot_7"), height = 600, width = 800))
            )
        )
    )
))