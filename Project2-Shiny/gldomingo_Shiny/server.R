# server.R

library(quantmod)
library(TTR)

# source("helpers.R")

# OPTIONAL TO LOAD ECONDATA FOR FUTURE CHARTING
# econdata <- readRDS("DATA/GDPCPIINT.rds")
# {str(econdata)
#   econdata[5,6]}

shinyServer(function(input, output) {
  
# TO MAKE PROCESSING REACTIVE
    randomVals<-eventReactive(input$go,{
      c(input$symb,input$symb2)
    })

# PREPARE UPPER PLOT
    output$plot1 <- renderPlot({
      data1 <- getSymbols(randomVals()[1], src = "yahoo",
      from = input$dates[1],
      to = input$dates[2],
      auto.assign = FALSE)

# DETERMINE CHARTTYPE
    charttype = ifelse(input$Candle,"candlestick","line")

# RENDER UPPER PLOT
    chartSeries(data1, name=randomVals()[1],theme = chartTheme("black"),
    type = charttype, up.col="white",dn.col="red2",
    log.scale = input$Log, TA = NULL)
    
  })
    
# PREPARE LOWER PLOT
    output$plot2 <- renderPlot({
      data2 <- getSymbols(randomVals()[2], src = "yahoo",
                  from = input$dates[1],
                  to = input$dates[2],
                  auto.assign = FALSE)

# RENDER LOWER PLOT               
      # chartSeries(data2, name=randomVals()[2],theme = chartTheme("black"),
      #             type = "bars", up.col="white",dn.col="red2",
      #             log.scale = input$Log, TA = NULL)
      barChart(data2,name=randomVals()[2], up.col="white",dn.col="red2")

# ADD TECHNICAL LAYERS TO LOWER PLOT
    if(input$RSI) {plot(addRSI())}
    if(input$Boll) {plot(addBBands())}
    if(input$MACD) {plot(addMACD())}
    
  })
})