## ui.R ##
library(shinydashboard)

shinyUI(dashboardPage(
  skin = "purple",
  header <- dashboardHeader(titleWidth = 450,
    title = "U.S. Land Cover Info System"
  ),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Land Type Map", tabName = "land_map", 
               icon = icon("map")),
      menuItem("Trend", tabName = "trends", 
               icon = icon("line-chart")),
      menuItem("State", tabName = "state", 
               icon = icon("map-pin")),
      menuItem("Data", tabName = "data", 
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
                column(6, selectizeInput("year", "Select Year", years))),
              fluidRow(
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
      
      tabItem(tabName = "state",
              fluidRow(
                column(6, selectizeInput("state", "Select State", state_name))),
              fluidRow(
                box(title = "Change Trend by State", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotlyOutput("BarPlot1"),
                    width = 12),
                box(title = "Distribution by State", 
                    status = "success", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotlyOutput("BarPlot2"),
                    width = 12)
              )
      ), #end tabItem 3
      
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
    ), #end tabItem 4
    tabItem(tabName = "about",
            fluidRow(
              column(width=12,
                     box(title = "About the Data",
                         status = "primary",
                         solidHeader=TRUE,
                         width = 12, collapsible=TRUE,
                         strong(br("The Land cover/use Data was downloaded from USDA-ERS (United States Department,
                         of Agriculture, Economic Research Service) data products center website at: "),
                         br(a("http://www.ers.usda.gov/data-products/major-land-uses.aspx",
                         href="http://www.ers.usda.gov/data-products/major-land-uses.aspx",target="_blank")),
                         br("The ERS has been dedicating on providing major land use estimates in the United States for over
                         50 years, collected from 14 Major Land Uses reports by region and state from 1945 to 2007.
                         14 Major Land Uses reports by region and state from 1945 to 2007.")
                     )),
                     box(title = "R Packages and Source Code",
                         status = "success",
                         solidHeader=TRUE,
                         width = 12, collapsible=TRUE,
                         strong(br("The R packages used in this app include:"),
                                br("shinydashboard, ggplot2, plotly, dplyr."),
                                br("Data tables and source code can be downloaded from the following Github repository:"),
                                br(a("https://github.com/fangbin08/usda",
                                href="https://github.com/fangbin08/usda",target="_blank"))
                     )),
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
