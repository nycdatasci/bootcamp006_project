# ---------------------------------------------------------------------------------
# Created by: Bernard Ong
# Created for: Shiny Project
# Project: Shiny Dashboard for Showcase (NTYC DSA 2nd Project Requirement)
# Purpose: Dashboard code (server side code)
# Critical for observeEvent >>> put isolate function just after each RENDER
# ---------------------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SHINY LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)
library(shinydashboard)
library(quantmod)
library(dygraphs)
library(xts)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SERVER SIDE CODE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output) { 

  # --------------------------------------------------------------
  # here are all the incoming input variables
  # --------------------------------------------------------------
  # sname --> stock/index ticker symbol
  # sparm --> stock parameter
  # wparm --> weather parameter
  # drange --> date range

  # --------------------------------------------------------------
  observe({

    # ---------------------------------------------------------------------
    # plot the time series dygraph - cross time series of stocks vs weather
    # ---------------------------------------------------------------------
    output$dygraph1 <- renderDygraph({
      # check for errors
      validate(
        need(input$drange[1]<input$drange[2], 'Please ensure the date range criteria is set properly.'),
        need(input$sname != "", 'Please enter a stock or index ticker symbol.')
      )
      # ----- get the stock history here
      sname <- toupper(input$sname)
      xdf1 <- getSymbols(sname, from=input$drange[1], to=input$drange[2],auto.assign=FALSE)
      colnames(xdf1) = c("Open","High","Low","Close","Volume","Adjusted")
      xdf2 <- getStockXts(input$sparm,xdf1)
      # ----- get weather info and convert to time series and filter on dates
      wdf2 <- getWeatherXts(input$wparm,wdf1)
      wdf2 <- wdf2[paste0(input$drange[1],'/',input$drange[2])]
      # now merge the two xts together 
      newser <- merge(xdf2,wdf2,join="inner")
      # now graph the dygraph here
      dygraph(newser, main = paste0("NYC Weather vs Stock Info ","(",toupper(input$sname),")")) %>%
        dyOptions(colors=RColorBrewer::brewer.pal(3, "Set2")) %>%
        dyAxis("x", label="Daily Movement") %>%
        dyAxis("y", label=paste0("NYC Weather Condition - ",input$wparm)) %>%
        dySeries(input$wparm, label=input$wparm) %>%
        dyLegend(width=300) %>%
        dyRoller(rollPeriod=1) %>%
        dySeries(input$sparm, axis="y2") %>%
        dyAxis("y2", label=paste0("Stock Info - ",input$sparm), independentTicks=FALSE)
    })

    # ---------------------------------------------------------------------
    # plot the scatter plot with linear regression line
    # ---------------------------------------------------------------------
    output$plot1 <- renderPlot ({
      # check for errors
      validate(
        need(input$drange[1]<input$drange[2], 'Please ensure the date range criteria is set properly.'),
        need(input$sname != "", 'Please enter a stock or index ticker symbol.')
      )
      # ----- get the stock history here
      sname <- toupper(input$sname)
      xdf1 <- getSymbols(sname, from=input$drange[1], to=input$drange[2],auto.assign=FALSE)
      colnames(xdf1) = c("Open","High","Low","Close","Volume","Adjusted")
      xdf2 <- getStockXts(input$sparm,xdf1)
      # ----- get weather info and convert to time series and filter on dates
      wdf2 <- getWeatherXts(input$wparm,wdf1)
      wdf2 <- wdf2[paste0(input$drange[1],'/',input$drange[2])]
      # now merge the two xts together 
      newser <- merge(xdf2,wdf2,join="inner")
      # we need to convert xts to dataframe 1st so it's easier for the R plot function
      newser <- newser[paste0(input$drange[1],'/',input$drange[2])]
      newser2 <- as.data.frame(newser)
      # set the plot param for stock info
      if (input$sparm=="Adjusted") {
        stockvar <- newser2$Adjusted }
      else 
        stockvar <- newser2$Volume
      # set the plot param for weather info
      if (input$wparm=="MaxTemp") {
        weathervar <- newser2$MaxTemp }
      else if (input$wparm=="MaxHum") {
        weathervar <- newser2$MaxHum }
      else if (input$wparm=="MaxWind") {
        weathervar <- newser2$MaxWind }
      else if (input$wparm=="MaxVis") {
        weathervar <- newser2$MaxVis }
      else if (input$wparm=="Cloud") {
        weathervar <- newser2$Cloud }
      else
        weathervar <- newser2$Rain
      # start the scatter plot
      plot(weathervar,
           stockvar,
           main=paste0("NYC ",input$wparm," vs Stock Info (",input$sname,") ",input$sparm),
           xlab=paste0("Weather Info - ",input$wparm),
           ylab=paste0("Stock Info - ",input$sparm),
           cex=2.0,               # dots size
           cex.main=1.5,          # plot title size relative to cex
           cex.lab=1.25,          # plot x and y labels size relative to cex
           pch=1,                 # dot type
           col="purple"           # dot color
      )
      abline(lm(stockvar ~ weathervar), col="red", lwd=2)
      # no need for this spline smoother, serves no purpose at the moment
      # lines(smooth.spline(newser2[weathervar],newser2[stockvar]), lty=2, lwd=5, col="green")
      
      # calculate all the correlations
      output$psCorr <- renderText ({
        # Pearson
        pscor <- cor(weathervar,stockvar,use="everything",method="pearson")
        getCorrScore(pscor)
      })
      output$kdCorr <- renderText ({
        # Kendall
        kdcor <- cor(weathervar,stockvar,use="everything",method="kendall")
        getCorrScore(kdcor)
      })
      output$smCorr <- renderText ({
        # spearman
        smcor <- cor(weathervar,stockvar,use="everything",method="spearman")
        getCorrScore(smcor)
      })
    })
 
  })

  # --------------------------------------------------------------
  #predicted <- reactive({

  #})
  
}
