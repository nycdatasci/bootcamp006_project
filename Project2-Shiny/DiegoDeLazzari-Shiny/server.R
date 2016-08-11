shinyServer(function(input, output,session){
  observe({
    # data frames
    if(input$whichData == 'Daily Arrivals'){
      data_stat = balkanRoute}
    else if(input$whichData == 'Gender'){
      data_stat = dataGender2016}
    else {data_stat = dataOrigin2016}

    # Column indexes
    if(input$whichData == 'Daily Arrivals'){
      col = col_balkanRoute}
    else if(input$whichData == 'Gender'){
      col = col_gender}
    else {col = col_demography}
    updateSelectInput(session, "selected", choices = col,
                    selected = col[1:min(4, length(col))]
                    )
    # show data using DataTable
    output$table <- renderDataTable({
      datatable(data_stat[,input$selected], rownames=FALSE,selection = 'multiple') %>% 
        formatStyle(input$selected, background="skyblue", fontWeight='bold')
                                  })
    })  
  
    # Create Map
    
  output$map <- renderPlotly({
    sel.data <- filter(balkanRoute.map, Date == monthStart(input$slider.map))
    sel.data2 <- filter(mediteRoute.map, Date == monthStart(input$slider.map))
     
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      scope='world',
      resolution = 50,
      projection=list(scale = 1),
      showframe = T,
      showcoastlines = T,
      projection = list(type = 'Mercator'),
      lataxis = list(range = c(30,50)),
      lonaxis = list(range = c(-10,40)),
      showsubunits = T,
      showcountries = T
    )
    plot_ly(sel.data, z = Arrivals, text = Country, locations = Code, type = 'choropleth',
            color = Arrivals, colors = 'Blues', marker = list(line = l),inherit = FALSE,
            colorbar = list(title = 'Balkans',xanchor = 'left')) %>%
      add_trace(.,type="scattergeo",
                locations = country_codes$Code, text = country_codes$Country, mode="text") %>%
      add_trace(., z = sel.data2$Arrivals, zmax = 25000, zmin = 500, text = sel.data2$Country, locations = sel.data2$Code, type = 'choropleth',
      color = sel.data2$Arrivals, colors = 'Greens', marker = list(line = l),inherit = FALSE,
      colorbar = list(title = 'Mediterranean', xanchor = 'right')) %>%
      layout(geo = g, widths = "100%")
             
  })
  

    output$origin <- renderPlotly({
      
      filt.dataOrigin2016 = filter(dataOrigin2016, Country == input$country) %>%
                            arrange(.,desc(Total.2015))
      
        p1= plot_ly(
        x = filt.dataOrigin2016$Origin,
        y = filt.dataOrigin2016$Total.2016,
        name = "2016",
        type = "bar") %>%

        add_trace(.,
        x = filt.dataOrigin2016$Origin,
        y = filt.dataOrigin2016$Total.2015,
        name = "2015",
        type = "bar") %>%
        layout(.,widths = "90%",
               yaxis = list(title = "Arrivals",type='log'), xaxis = list(title = "", tickangle=-90))
     })
  
    
    output$gender <- renderPlotly({
      
      filt.dataGender2016 = filter(dataGender2016, Country == input$country)
      
      p2 = plot_ly(
        x = filt.dataGender2016$Group,
        y = filt.dataGender2016$Total.2016,
        name = "2016",
        type = "bar") %>%
        
        add_trace(.,
                  x = filt.dataGender2016$Group,
                  y = filt.dataGender2016$Total.2015,
                  name = "2015",
                  type = "bar") %>%
        layout(., barmode = "stack",
               yaxis = list(title = "Arrivals"), xaxis = list(title = "",tickangle=-90))
    })
    
    # Plot time series
    
    
    
    output$arrivals_by_day <- renderDygraph({
      dygraph(balkanTimeSeries) %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE) %>% 
        dyRangeSelector() %>%
        dyRoller(rollPeriod = 7)
      
    })
    
})