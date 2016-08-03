library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(threejs)
library(googleVis)

shinyUI(
    dashboardPage(skin = "black",
        dashboardHeader(title = "World Data",disable = TRUE),
        dashboardSidebar(
            sidebarUserPanel(h4(href = "#", icon("fire"), class = "", "   LPG Tracker")),
            sidebarMenu(id = "menu",
                menuItem("Map", tabName = "map", icon = icon("map")),
                menuItem("Flow Chart", tabName = "flow", icon = icon("arrows-h")),
                menuItem("Data", tabName = "data", icon = icon("database")),
                menuItem("Info", tabName = "info", icon = icon("info")),
                HTML('<hr style="color: purple;">'),
                HTML('<h4 style="padding-left: 12px; padding-bottom: 0px; margin: 0px;">Filters</h4>'),
                # uiOutput("daterange"),
                radioButtons('radio', '', c('Source Country'='Source..1.','Destination Country'='Primary.Destination.Country')),
                uiOutput("filter"),
                uiOutput("theme")
            )
        ),
        dashboardBody(
            #custom css styling
            tags$head(tags$style(HTML('
              .main-sidebar {
                  padding-top: 0px !important
              }
              #globe {
                  height: calc(105vh) !important;
                  margin-top: -20px !important
              }
              #slider {
                  position: fixed !important;
                  bottom: 50px !important;
                  left: 30% !important;
                  width: 60% !important;
              }
              #slider .shiny-input-container:not(.shiny-input-container-inline) {
                  width: 100% !important;
              }
              .content > .shiny-input-container{
                  position: fixed !important;
                  bottom: 50px !important;
                  left: 275px !important;
                  width: 78% !important;
              }
              #shiny-tab-map > .row:first-child {
                  position: fixed !important
              }
              .small-box > .inner {
                  width: 250px;
                  padding-left: 30px  
              }
              .bg-purple {
                  top: 140px;
                  left: -33px
              }
              .bg-orange {
                  top: 260px;
                  left: -66px
              }
              .box{
                  margin-top: 10px
              }
              #sankey{
                  margin-top:25px !important;
                  width: 100% !important;
                  height: 80% !important
              }
              .hide {
                  visibility: hidden
              }
              .show {
                  visibility: visible
              }
            '))),
            tabItems(
                tabItem(tabName = "map",
                    fluidRow(
                        column( width=5,
                            tags$div(
                                 valueBoxOutput("value")
                            ),
                            tags$div(
                                valueBoxOutput("value1")
                            ),
                            tags$div(
                                valueBoxOutput("value2")
                            )
                        )
                    ),
                    fluidRow(
                        globeOutput("globe")
                    )
                ),
                tabItem(tabName = "flow",
                    fluidRow(
                        column(
                            HTML('<p style="font-style: italic;position: absolute">Exports</p>'),
                            HTML('<p style="left: 38%;font-style: italic;position: absolute">*Only shows top 25 connections</p>'),
                            HTML('<p style="right: 20px; font-style: italic;position: absolute">Imports</p>'),
                            HTML('<h4 id="error" style="position: absolute; visibility:hidden; left: 43%; top: 40%;">No results</h4>'),
                            htmlOutput("sankey"),
                            width=12,
                            height=600
                        )
                    )
                ),
                tabItem(tabName = "data",
                    fluidRow( height='70%',
                        column(
                            ##TODO add id to box and make window height
                            DT::dataTableOutput("table"),
                            width=12,
                            height='100%'
                        )
                    )
                ),
                tabItem(tabName = "info",
                    fluidRow(
                        box(width=12,
                            HTML("<h2>LPG Tracker</h2>"),
                            HTML("<h3>Purpose</h3>"),
                            HTML("<p>This application is meant to visualize the flow of transported LPG over seas between countries.</p>"),
                            HTML("<h3>Pages</h3>"),
                            HTML("<h4>Map</h4>"),
                            HTML("<p>The map page displays arcs indicating trade of LPG defined as C3 + C4 from country to country. 
                                 The more green and the wider the arc the greater the volume transported.</p>"),
                            HTML("<h4>Flow Chart</h4>"),
                            HTML("<p>A sankey chart visualizing the import and exports between countries filter  by the data range slider and selectors located in the side bar menu.</p>"),
                            HTML("<h4>Data</h4>"),
                            HTML("<p>A table displaying the data filtered by the data range slider and selectors located in the side bar menu.</p>"),
                            HTML("<h3>Sources</h3>"),
                            HTML("<a href='https://developers.google.com/public-data/docs/canonical/countries_csv'>Google Countries Lat and Long Dataset</a></br>"),
                            HTML("<a href='https://www.marketintelligencenetwork.com/'>IHS liftings data for volume tranported</a>")
                        )
                    )
                )
            ),
            sliderInput("slider", "", 
                min(ihs$Date, na.rm=TRUE),
                max(ihs$Date, na.rm=TRUE),
                value= c(min(ihs$Date, na.rm=TRUE), min(ihs$Date, na.rm=TRUE) + 30),
                step = 7,
                timeFormat='%v',
                animate=animationOptions(interval=250, loop=T)
            ),
            #autoplay animation slider on app load
            tags$script("$(document).ready(function(){
                $('.slider-animate-button').click(function(){
                    $('.col-sm-5').toggleClass( 'hide', 2000, 'easeOutSine')
                });
                if ($('#google-visualization-errors-all-3').length == 1) {
                    $('#error').toggleClass( 'show', 2000, 'easeOutSine')
                }
                if ($('#google-visualization-errors-all-1').length == 1) {
                    $('#error').toggleClass( 'show', 2000, 'easeOutSine')
                }
            });")
        )
    ) 
)
