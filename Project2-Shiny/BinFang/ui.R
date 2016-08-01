## ui.R ##
library(shinydashboard)

shinyUI(dashboardPage(
  skin = "purple",
  header <- dashboardHeader(
    title = "U.S. Land Cover"
  ),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Land Type Map", tabName = "land_map", 
               icon = icon("map")),
      menuItem("Trends", tabName = "trends", 
               icon = icon("line-chart")),
      menuItem("Data Table", tabName = "data", 
               icon = icon("database")),
      menuItem("About", tabName="about", 
               icon = icon("info"))
      ),
    selectizeInput("landtype","Land Cover Type",
                   choices = landtypes,
                   selected = "Crop")
    ), #end sideBar
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "land_map",
              fluidRow(
                selectizeInput("year", "Select Year", years),
                box(title = "Land Cover Map by State", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotlyOutput("outputmap"),
                    width = 12),
                box(title = "Land Cover Ranking by State", 
                    status = "success", collapsible = TRUE,
                    plotlyOutput("rankingplot"),
                    width = 12, solidHeader = TRUE)
           )
      ), #end tabItem 1
      
      tabItem(tabName = "trends", 
              fluidRow(
                column(width = 12, 
                       box(title = "Change Trend between 1945-2007",
                         status = "primary",
                         collapsible = TRUE,
                         plotlyOutput("TrendPlot1"), 
                         width = NULL, solidHeader = TRUE)
        ),
                column(width = 12, 
                       box(title = "Correlation between Crop, Forest and Grass",
                           selectizeInput("year_cor", "Select Year", years),
                           status = "success",
                           collapsible = TRUE,
                           plotlyOutput("TrendPlot2"), 
                           width = NULL, solidHeader = TRUE)
        )
        )
      ), #end tabItem 2
      
      tabItem(tabName = "data", 
              tabsetPanel(
                tabPanel("Land Cover (Percentage)", #tabPanel 1
                         fluidRow(
                           column(width=12,
                                  box(title = "Land Cover by State (Percentage)", 
                                      solidHeader=TRUE,
                                      width = 12, collapsible=FALSE,
                                      div(style = 'overflow-x: scroll', 
                                          dataTableOutput("table_rate"))
                                  )
                           )) #end fluidRow
                ), #end tabPanel 1
                tabPanel("Land Cover(Million Acres)", #tabPanel 2
                         fluidRow(
                           column(width=12,
                                  box(title = "Land Cover by State (Acres)", 
                                      solidHeader=TRUE,
                                      width = 12, collapsible=FALSE,
                                      div(style = 'overflow-x: scroll', 
                                          dataTableOutput("table_acre"))
                                  )
                           )) #end fluidRow
                ) #end tabPanel 2
              ) #end tabsetPanel
    ), #end tabItem 3
    tabItem(tabName = "about",
            fluidRow(
              column(width=12,
                     box(title = "Introduction",
                         status = "primary",
                         solidHeader=TRUE,
                         width = 12, collapsible=TRUE,
                         strong(paste(intro_text1))
                     ),
                     box(title = "R packages and Source Code",
                         status = "success",
                         solidHeader=TRUE,
                         width = 12, collapsible=TRUE,
                         strong(paste(intro_text2))
                     ),
                     box(img(
                       src="http://www.aaea.org/UserFiles/image/USDAERS_v1.png",
                       height = 150, width = 300
                     )),
                     box(img(
                       src="http://parishenrycoedc.com/wp-content/uploads/2014/08/USDA.jpg",
                       height = 200, width = 300
                     )
                     )
            )
         )
    )
  ) #end all tabItems
  ) #end dashboardBody
))
