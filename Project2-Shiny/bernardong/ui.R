# ---------------------------------------------------------------------------------
# Created by: Bernard Ong
# Created for: Shiny Project
# Project: Shiny Dashboard for Showcase (NTYC DSA 2nd Project Requirement)
# Purpose: Dashboard code (ui code)
# ---------------------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SHINY LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)
library(shinydashboard)
library(dygraphs)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI SIDE CODE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# -----------------------------------------------------------
# Dashboard Header
# -----------------------------------------------------------
header <- dashboardHeader(
  title = "Bubo Stock Analyzer",
  # titleWidth = 200,   # --> only if needed later
  disable = FALSE
)

# -----------------------------------------------------------
# Dashboard Sidebar
# -----------------------------------------------------------
sidebar <- dashboardSidebar(
  # width = 200,      # --> only if needed - need to make this same as the header width
  
  # Input the stock or index symbol here - convert to upper case
  textInput("sname", "Enter Stock/Index Symbol:", value = "^GSPC", width="100%"),
  
  # radio buttons to choose between Adjusted Closing Price or Transaction Volume
  radioButtons("sparm", "Select Stock/Index Data:",
               c("Adjusted Closing Price" = "Adjusted",
               "Transaction Volume" = "Volume"),
               width = "100%"
  ),
  #hr(),
  
  # dropdown menu to choose between: 
  # Max Temp, Max Hum, Max Wind, Cloud Cover (oktas 0-8), Max Visibility and Rain
  selectInput("wparm", "Select Weather Data:",
              c("Max Temperature (F)" = "MaxTemp",
              "Max Humidity (%)" = "MaxHum",
              "Max Wind Velocity (mph)" = "MaxWind",
              "Max Visibility (mi)" = "MaxVis",
              "Cloud Cover (oktas)" = "Cloud",
              "Rain (in)" = "Rain"),
              width = "100%"
  ),
  #hr(),
  
  # Date range selector, with earliest 2013-01-01, and latest 2015-12-31
  dateRangeInput("drange", "Choose Date Range:",
                 start = "2013-01-01",
                 end   = "2015-12-31",
                 min = "2013-01-01",
                 max   = "2015-12-31",
                 format = "yyyy-mm-dd",
                 width = "100%"
  ),
  br(), br(),
  div(img(src="bubo.png"),style="text-align:center;")

  #hr(),
  # press submit button labeled "Analyze"
  #br(),
  #p(actionButton("abut", "Initialize"),align="center"),
  #p("To Establish the Connection",align="center")

)

# -----------------------------------------------------------
# Dashboard Body
# -----------------------------------------------------------

body <- dashboardBody(
  fluidRow(
    column(width=12,
      # Put content here on this row, taking all of 12 columns
      tabBox(
        title = "by Bernard Ong",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "100%", width = "100%",
        # Tab 1 ---------------------------------------------
        tabPanel(strong("TRENDING"), 
          box(
            width = "100%",
            heigth = "100%",
            title = "Time Series Charts",
            status = "primary",
            solidHeader = TRUE,
            dygraphOutput("dygraph1",height="600")
          )        
        ),
        # Tab 2 ---------------------------------------------
        tabPanel(strong("CORRELATOR"), 
          box(
            width = "100%",
            heigth = "100%",
            infoBox("Pearson",textOutput("psCorr"),icon=icon("list"),color="aqua"),
            infoBox("Kendall",textOutput("kdCorr"),icon=icon("list"),color="purple"),
            infoBox("Spearman",textOutput("smCorr"),icon=icon("list"),color="yellow")
          ),
          box(
            width = "100%",
            height = "100%",
            title = "Scatter Plots",
            status = "success",
            solidHeader = TRUE,
            plotOutput("plot1")
          )
        ),
        # Tab 3 ---------------------------------------------
        tabPanel(strong("ABOUT"), 
          box(
            width = "100%",
            height = "100%",
            title = "Reference",
            status = "info",
            solidHeader = TRUE,
            h4("About the Bubo Stock Analyzer"),
            p("There have been uncertainties in the research community as to whether weather really
              affects the financial markets. On one side, it said that there is absolutely no
              correlation, and on the other side, some say there is. This is an area in behavioral
              finance that deserves some more investigation. This analyzer tool is created to provide
              you with an easy way to look at how a stock or index price in the market trends in
              conjunction with different weather patterns, and provides a correlation strength
              between the two, allowing you to see for yourself if weather really affects the 
              equities market or not."),
            h4("Data Sources"),
            p("The data are extracted from two different sources. The stock histories are streamed 
              in dynamically and the weather data is locally stored in a text file. All historical
              information spans the course of 3 years, from 2013 through 2015, which should be
              sufficient for analysis purposes. The data is based on a daily movement time series.
              I will continue to add to the historical data as needed."),
            HTML("<ul>
                 <li>Yahoo Finance (for stock and index history information)</li>
                 <li>Underground Weather (for all weather conditions history, limited only to NYC)</li>
                 </ul>"),
            h4("Stock Data"),
            p("Here are 2 types of stock and index data included for use:"),
            HTML("<ul>
                 <li>Adjusted Closing Price (in $)</li>
                 <li>Transaction Volume (in shares)</li>
                 </ul>"),
            h4("Weather Data"),
            p("Here are the different types of weather data included for use:"),
            HTML("<ul>
                 <li>Maximum Temperature (in F)</li>
                 <li>Maximum Humidity (in %)</li>
                 <li>Maximum Visibility (in miles)</li>
                 <li>Maximum Wind Speed (in mph)</li>
                 <li>Rain or Precipitation (in inches)</li>
                 <li>Cloud Cover (in oktas)</li>
                 </ul>"),
            h4("Cloud Cover Reference Data"),
            img(src="cloud-oktas.png"),
            h4("Correlation Strengths"),
            p("The strength of a correlation between a weather condition type against a stock or index
              price or transaction volume data is measured using Pearson, Kendall, and Spearman.
              Every correlation has two qualities: strength and direction. The direction of a 
              correlation is either positive or negative. In a negative correlation, the weather condition
              vs the stock data move in inverse, or opposite, directions. When stock and weather data 
              have a positive correlation, it means they move in the same direction. This means that 
              as one side increases, so does the other one."),
            p("For data exploration, it would be interesting to see all three correlations from 
              Pearson, Kendall, and Spearman. As a simple guide, Pearson is normally used when
              there seems to be more a linear relationship (interval data) or better normal 
              distribution in the data points. Kendall or Spearman is used more on ranks (ordinal 
              type data, e.g. cloud cover comes to mind here) and depicts a monotonic relationship. 
              Kendall might also be better over Spearman in some nonlinear cases. But the three
              are all presented for an interesting comparison. When the Spearman number is greater 
              than Pearson, that would indicate that the correlation mostly likely is not linear."),
            p("Here is a crude estimate of correlation strengths to provide further guidance:"),
            img(src="corr-desc.png"),
            hr(),
            h4("Contact Information"),
            p("Bernard Ong, Data Scientist"),
            p("Email: bernard.ong@entense.com"),
            p("Cell: 201-916-5241"),
            p("")
          )        
        )
      )
    )
  )
)

# -----------------------------------------------------------
# Render the UI
# -----------------------------------------------------------
#ui <- dashboardPage(header, sidebar, body)
ui <- dashboardPage(header, sidebar, body, skin = "yellow")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DASHBOARD INSTANTIATION & LAUNCH
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# shinyApp(ui, server)
