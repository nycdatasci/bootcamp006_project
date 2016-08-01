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
      # print("test")
      data=ratio_data[[input$landtype]][,c("code", input$year, "hover"), drop = FALSE]
      names(data) <- c("code", "year", "hover")
      plot_ly(data, z = year, text = hover, locations = code, type = 'choropleth',
                   width = "auto", height = "auto",
      locationmode = 'USA-states', color = year, colors = palettes[input$landtype],
      marker = list(line = l), colorbar = list(title = "Percentage"))
      layout(title = "", geo = g)
    })
  
  # Draw Ranking Plot  
    output$rankingplot = renderPlotly({
    ratio_data[[input$landtype]][4:17] # Draw ranking plot
        # print(col_year)
    ranking = data.frame(as.character(codes[1:50]),
                         ratio_data[[input$landtype]][input$year][1:50,])
        # print(ranking)
        names(ranking) <- c("State", "Percentage")
        ranking_sort <- ranking[order(-ranking$Percentage),] 
      
      plot_ly(ranking_sort, x = State, y = Percentage, 
              name = 'Percentage within State',
              type = "bar", title = 'Ranking', 
              marker = list(color = toRGB("orange"))) %>% 
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
      Crop = acre_data_top3$crop[,input$year_cor]
      y = acre_data_top3$forest[,input$year_cor]
      z = acre_data_top3$grass[,input$year_cor]
      plot_ly(x = x, y = y, z = z,
              text = paste("State: ", acre_data_top3$crop$state),
              color = acre_data_top3$crop$region,
              type="scatter3d", mode="markers") %>%
        layout(scene = list(xaxis = list(title = "Crop"), 
                            yaxis = list(title = "Forest"), 
                            zaxis = list(title = "Grass")), 
                            title = "")
    })
    
    # Draw Tables
    output$table_rate <- renderDataTable(var)
    output$table_acre <- renderDataTable(var_acre)
  }
)