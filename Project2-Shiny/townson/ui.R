library(shinydashboard)
library(googleVis)
library(DT)

shinyUI(dashboardPage(
  dashboardHeader(title = "Natural Gas"),
  dashboardSidebar(
    sidebarUserPanel("Production and Consumption"),
    sidebarMenu(id="menu",
                menuItem("Natural Gas Withdrawals Map", tabName = "map_tot",icon=icon("map")),
                menuItem("Natural Gas Consumption Map", tabName = "map_con", icon=icon("map-o")),
                menuItem("Charts", tabName = "charts",icon=icon("line-chart")),
                uiOutput('filter'),
                uiOutput('filter1'),
                uiOutput('filter2'),
                uiOutput('filter3')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="map_tot",
              verticalLayout(
                box(htmlOutput("map"),height=600,width=1000),
                box(htmlOutput("mapData"),height=200, width=800)),
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 220,
                            width = 220, height = "auto",
                            textOutput("total_US"),
                            plotOutput("other_regions",height=300))),
      tabItem(tabName="map_con",
              verticalLayout(
                box(htmlOutput("map_cons"),height=600,width=1000),
                box(htmlOutput("map_consData"),height=200, width=800)),
              absolutePanel(id = "controls2", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 220,
                            width = 220, height = "auto",
                            textOutput("total_con_US"),
                            plotOutput("other_con_regions",height=300))),
      tabItem(tabName="charts",
              verticalLayout(
                box(plotOutput("chart"),height=400,width=1000),
                box(plotOutput("chart2"),height=400,width=1000)
              ))
    )
  )
))