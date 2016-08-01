
source("helper.R")
library(rglwidget)
library(rbokeh)
library(plotly)
library(rgdal)
library(threejs)
library(shinyGlobe)

shinyServer(
  function(input, output, session){
    
    ####
    ghg.df <- reactive({
      print(input$wquestion)
      subset(ghg, Variable == input$wquestion & Year == input$wyear)
    })
    ####
    output$worldmap <- renderPlot({
      WorldMapViz(ghg.df()) 
    })
    
    output$scatterplot<- renderPlotly({
        carbon<-grand_data[, input$carboninput]
        econ<-grand_data[, input$gdpinput]
        plot_ly(x=econ, y=carbon, mode="markers",
                text = grand_data$Country , color = grand_data$Country, 
                size = econ)
    })
    
        output$globe <- renderGlobe(

         globejs( img = "./www/RenderData.jpeg", atmosphere = TRUE, bg="black")

        )

    output$yearSlider <- renderUI({
      #years <- getYears.World()
      sliderInput("wyear", label = "Year:", 
                  min=min(1995), max=max(2011), value = 1995, step = 1, sep = "")  
    })

    output$timeplot <-renderDygraph({
      print(1)
      dygraph(avg, main="World Average Temperature") %>%
        dySeries(name='V3', label = 'average temperature') %>%
        dyRangeSelector(height = 20)
    })

   #########################################
    dataInput<-reactive({
      data <- getSymbols(input$symb, src = "yahoo", 
                         from = input$dates[1],
                         to = input$dates[2],
                         auto.assign = FALSE)
    })
    
    finalInput<-reactive({
      if(!input$adjust) return(dataInput())
      return(adjust(dataInput()))
    })
    
    output$plot <- renderPlot({
      data <- finalInput()
      print (data)
      chartSeries(data, theme = chartTheme("white"), 
                  type = "line", log.scale = input$log, TA = NULL)
    })
    
    output$text1<-renderUI({
      "Try this sentence pls!"
    })
    

    

    
  }
)