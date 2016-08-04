## server.R ##
library(shiny)
library(albersusa)
library(ggplot2)
library(plotly)
library(dplyr)

shinyServer(
  function(input, output) {
  # Draw Map  
    output$outputmap = renderPlotly({
      data=ratio_data[[input$landtype]][,c("code", input$year, "hover"), drop = FALSE]
      names(data) <- c("code", "value", "hover")
      state_rank = rank(-data$value)
      state_acre = acre_data[[input$landtype]][,input$year]
      plot_ly(data, z = value, text = paste(hover, "<br>", state_acre, "Million Acres", "<br>", 
                                            "Rank:", state_rank), 
              locations = code, type = 'choropleth',
                   width = "auto", height = "auto",
      locationmode = 'USA-states', color = value, colors = palettes[input$landtype],
      marker = list(line = l), colorbar = list(title = "Percentage"))
      layout(title = "", geo = g)
    })
  
  # Draw Ranking Plot  
    output$rankingplot = renderPlotly({
    ratio_data[[input$landtype]][4:17] # Draw ranking plot
    ranking = data.frame(as.character(codes[1:50]),
                         ratio_data[[input$landtype]][input$year][1:50,])
        names(ranking) <- c("State", "Percentage")
        ranking_sort <- ranking[order(-ranking$Percentage),] 
      
      plot_ly(ranking_sort, x = State, y = Percentage, 
              name = 'Percentage within State',
              type = "bar", title = 'Ranking', marker = list(color = toRGB("orange"))) %>% 
              add_trace(x = State, y = Percentage, 
              name = "spline",line = list(shape = "linear"),
              marker = list(color = toRGB("blue4"))) %>%
        layout(title = "", showlegend = F)
     })

  # Draw Time-series Plot 1
    output$TrendPlot1 = renderPlotly({
      tseries1_data <- t_transpose(ratio_data[[input$landtype]])
      x <- list(title = "Year")
      y <- list(title = "Percentage")
      p <- plot_ly(tseries1_data, x= year, y=Northeast, name = 'Northeast')
      p <- add_trace(p, x= year, y=Lake.States, name = 'Lake States')
      p <- add_trace(p, x= year, y=Corn.Belt, name = 'Corn Belt')
      p <- add_trace(p, x= year, y=Northern.Plains, name = 'Northern Plains')
      p <- add_trace(p, x= year, y=Appalachian, name = 'Appalachian')
      p <- add_trace(p, x= year, y=Southeast, name = 'Southeast')
      p <- add_trace(p, x= year, y=Delta.States, name = 'Delta States')
      p <- add_trace(p, x= year, y=Southern.Plains, name = 'Southern Plains')
      p <- add_trace(p, x= year, y=Mountain, name = 'Mountain')
      p <- add_trace(p, x= year, y=Pacific, name = 'Pacific')
      p <- add_trace(p, x= year, y=Others, name = 'Others') %>%
        layout(xaxis = x, yaxis = y, showlegend = T, 
               title = "")
      p
    })
    
    # Draw 3d scatter plot
    output$TrendPlot2 = renderPlotly({
      x = acre_data_top3$crop[,input$year_cor]
      y = acre_data_top3$forest[,input$year_cor]
      z = acre_data_top3$grass[,input$year_cor]
      plot_ly(x = x, y = y, z = z,
              text = paste("State: ", acre_data_top3$crop$state),
              color = acre_data_top3$crop$region,
              type="scatter3d", mode="markers",colors= 
                c("#ff0000", "#ff8000", "#ffbf00", "#ffff00","#80ff00","#00ff80", 
                  "#00ffff", "#0080ff", "#0040ff","#8000ff","#ff00ff")) %>%
        layout(scene = list(xaxis = list(title = "Crop"), 
                            yaxis = list(title = "Forest"), 
                            zaxis = list(title = "Grass")), 
                            title = "")
    })
    
    # Draw Barplot1
    output$BarPlot1 = renderPlotly({
      x = value_list$year
      y = value_list[,input$state]
      type = value_list$type
      plot_ly(x=x, y=y, type = "bar", color = type) %>%
        layout(xaxis = list(title = ""), 
               yaxis = list(title = "Percent"), 
               title = "")
    })
    
    # Draw Barplot2
    output$BarPlot2 = renderPlotly({
      y = value_list[,input$state]
      type = value_list$type
      plot_ly(y=y, type = "box", color = type) %>%
        layout(xaxis = list(title = "Land Type"), 
               yaxis = list(title = "Percent"), 
               title = "")
    })
    
    # Draw Tables
    output$table_rate <- renderDataTable(ratio_data_nohover[[input$landtype]])
    output$table_acre <- renderDataTable(ratio_acre_nohover[[input$landtype]])
  }
)