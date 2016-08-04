library(shiny)

# GREG'S UI FOR SHINY APP WITH YAHOO

shinyUI(fluidPage(
  
# theme = "bootstrap.min.css", from Bootswatch
  
  theme = "slate.css",
  
# titlePanel("Analyzer"),

  sidebarLayout(
    sidebarPanel(width=3,

# LAYOUT UPPER SIDEBAR
      tags$h1("Basic Stock & FX Technical Analyzer"),
      tags$h6("Information courtesy of Yahoo Finance!"),
      tags$h6("Style courtesy of Bootswatch"),
      helpText("Today is",Sys.Date()),
      
# LAYOUT MAIN INPUT AREA 
      helpText("Input stocks or currencies to explore"), 
      textInput("symb", "Symbol:", "^DJI"),
      textInput("symb2", "Symbol2:", "^IXIC"),
      dateRangeInput("dates", 
        "Enter Date Range",
        start = "2016-01-01", 
        end = as.character(Sys.Date())
      ),
      actionButton("go",strong("RENDER GRAPHS"),style = "color:yellow;"),

# LAYOUT FOR OPTIONS CHOICES
      helpText("Select For Upper Display:"),
      tags$h6("Default Chart is a Line Plot"),
      checkboxInput("Candle","Candlestick Chart", value = FALSE),
      checkboxInput("Log","Log Scale", value = FALSE),
      helpText("Technical Analysis Layers For Bottom Display"),
      checkboxInput("RSI","RSI", value = FALSE), 
      checkboxInput("Boll","Bollinger Bands", value = FALSE),
      checkboxInput("MACD","MACD", value = FALSE)

    ),
    
    mainPanel(
      fluidRow(
        plotOutput("plot1")
      ),

      fluidRow(
        plotOutput("plot2")
      )
    )
  )
))