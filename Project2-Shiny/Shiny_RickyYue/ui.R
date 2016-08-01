library(shiny)
library(leaflet)
library(ggvis)

ui = fluidPage(
  titlePanel("European Soccer Leagues"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sea",label="Season",
                  choices=c("2008/2009","2009/2010","2010/2011","2011/2012","2012/2013","2013/2014","2014/2015","2015/2016"),
                  selected="2015/2016"),
      selectInput("lea",label="League",
                  choices=c("England","Spain","Italy","Germany","France","Scotland","Portugal","Netherlands"),
                  selected="England")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Soccer League Map",leafletOutput("myMap")),
        tabPanel("Trend by Season",plotOutput("myLinechart"))
      )
    )
  )
)