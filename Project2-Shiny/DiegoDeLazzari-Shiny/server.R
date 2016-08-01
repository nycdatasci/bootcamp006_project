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
    sel.data <- filter(balkanRoute.map, Date == input$slider.map)
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      scope='world',
      projection=list(scale = 1),
      showframe = T,
      showcoastlines = T,
      projection = list(type = 'Mercator'),#''Orthographic
      lataxis = list(range = c(30,50)),
      lonaxis = list(range = c(0,40)),
      showsubunits = T,
      showcountries = T
    )
    plot_ly(sel.data, z = Arrivals, text = Country, locations = Code, type = 'choropleth',
            color = Arrivals, colors = 'Blues', marker = list(line = l),inherit = FALSE,
            colorbar = list(title = 'Arrivals')) %>%
      add_trace(.,type="scattergeo",
                locations = country_codes$Code, text = country_codes$Country, mode="text") %>%
      layout(title ='Daily arrivals across the Balkans', geo = g, width = 900)
             
  })
  

    output$origin <- renderPlotly({
      
      filt.dataOrigin2016 = filter(dataOrigin2016, Country == input$country)
      
      p1 = plot_ly(
        x = filt.dataOrigin2016$Origin,
        y = filt.dataOrigin2016$Total.2016,
        name = "2016",
        type = "bar") %>%

        add_trace(.,
        x = filt.dataOrigin2016$Origin,
        y = filt.dataOrigin2016$Total.2015,
        name = "2015",
        type = "bar") %>%
        layout(.,title = "Country of origin",  width = 480, height = 280,
               yaxis = list(title = "Arrivals",type='log'), xaxis = list(title = ""),tickangle=-90)
     })
    # p1 = plot_ly(data=dataOrigin2016, labels = filt.dataOrigin2016$Origin, domain = list(x = c(0, 0.4), y = c(0.4, 1)),
    #               name = '2016', values = filt.dataOrigin2016$Total.2016, type = "pie", showlegend = F) %>%
    #       add_trace(data = dataOrigin2016, labels = filt.dataOrigin2016$Origin, domain = list(x = c(0.4, 1), y = c(0.4, 1)),
    #              name = '2015', values = filt.dataOrigin2016$Total.2015, type = "pie", showlegend = F) %>%
    #      layout(title = "Country of origin",  width = 480, height = 280)
    #   })
    
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
        layout(.,title = "Demographics",  width = 480, height = 280, barmode = "stack",
               yaxis = list(title = "Arrivals"), xaxis = list(title = ""),tickangle=-90)
    })
    
    # Plot time series
    
    
    
    output$arrivals_by_day <- renderDygraph({
    # Show only Greece, Croatia and Austria
      dygraph(balkanTimeSeries[,c(1,4,5)]) %>%
        # dyOptions(logscale=TRUE) %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE) %>% 
        dyRangeSelector() %>%
        dyRoller(rollPeriod = 7)
      
    })
    
})
#########################
# show Destination map using googleVis   "Arrivals by Country of origin"
# output$destination <- renderGvis({
#     gvisGeoChart(balkanRoute, "state.name", input$selected,
#                  options=list(region="", displayMode="regions",
#                               resolution="provinces",
#                               width="auto", height="auto"))
# })
# 
#     # show statistics using infoBox
#     output$maxBox <- renderInfoBox({
#         max_value <- max(data_stat[,input$selected])
#         max_state <- 
#             data_stat$state.name[data_stat[,input$selected] == max_value]
#         infoBox(max_state, max_value, icon = icon("hand-o-up"))
#     })
#     output$minBox <- renderInfoBox({
#         min_value <- min(data_stat[,input$selected])
#         min_state <- 
#             data_stat$state.name[data_stat[,input$selected] == min_value]
#         infoBox(min_state, min_value, icon = icon("hand-o-down"))
#     })
#     output$avgBox <- renderInfoBox(
#         infoBox(paste("AVG.", input$selected),
#                 mean(data_stat[,input$selected]), 
#                 icon = icon("calculator"), fill = TRUE))
#     
# Legend
# observe({
#   proxy <- leafletProxy("map", data = balkanRoute.map)
#   
#   # Remove any existing legend, and only if the legend is
#   # enabled, create a new one.
#   proxy %>% clearControls()
#   if (input$legend) {
#     pal <- colorpal()
#     proxy %>% addLegend(position = "bottomright",
#                         pal = pal, values = ~Arrivals
#     )
#   }
# })
# 
# This reactive expression represents the palette function,
# which changes as the user makes selections in UI.
# colorpal <- reactive({
#   colorNumeric(input$colors, balkanRoute.map$Arrivals)
# })

# #creating reactive function for selected options
# sel.spec <-reactive({ specs = paste("<strong>", sel.data()$Arrivals,"</strong>")
# })
# 
# 
# 
#### Create the map
# 
# #creating reactive functions for selected Date
# sel.data <- reactive({
#   filter(balkanRoute.map, Date == input$slider.map)
# })
# 
# # Create the static part of the map
# output$map <- renderLeaflet({
#   leaflet(balkanRoute.map) %>%
#     addProviderTiles("CartoDB.Positron") %>%
#     setView(lat = 42, lng = 20, zoom = 5) %>%
#     addCircles(~lon, ~lat, color = "red", radius = 50000, weight = 2,
#                fillOpacity = 0.3) %>%
#     addPolylines(geo_loc$lon[-5], geo_loc$lat[-5],color = "red", 
#                  weight = 2, fillOpacity = 0.7)%>%
#     addPolylines(geo_loc$lon[c(3,5,7)], geo_loc$lat[c(3,5,7)],color = "red",
#                  weight = 2, fillOpacity = 0.7)
# })
# 
# # Create the reactive part of the map
# observe({
#   # pal = colorpal()
#   leafletProxy("map", data = sel.data()) %>%
#     clearShapes() %>%
#     addCircles(~lon, ~lat, radius = 50000, color = "red",
#                 fillOpacity = 0.3) %>%
#     addPopups(~lon, ~lat, ~paste(Country,': ',Arrivals), 
#               options = popupOptions(minWidth = 10, closeOnClick = FALSE, 
#                                      closeButton = FALSE))%>%
#    addPolylines(geo_loc$lon[-5], geo_loc$lat[-5],color = "red", 
#                 weight = 2, fillOpacity = 0.7)%>%
#    addPolylines(geo_loc$lon[c(3,5,7)], geo_loc$lat[c(3,5,7)],color = "red",
#                 weight = 2, fillOpacity = 0.7)
# })