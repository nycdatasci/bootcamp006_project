#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


Incomes = select(ACS, PUMA, Year, HINCP) %>% 
  filter(PUMA %in% d10s$`d10comb$PUMA`) %>%
  filter(as.integer(PUMA) > 3700)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table = DT::renderDataTable(DT::datatable({
    data = read.csv('data/display_tab.csv')
    data = select(data, PUMA..Code, Borough, Name)
    data
  }))
  
  # output$mychart = renderLineChart({
  #   # Return a data frame. Each column will be a series in the line chart.
  #   if(input$first == ''){char1 = '03701'} else {char1 = input$first}
  #   if(input$second == ''){char2 = '03702'} else {char2 = input$second}
  #   if(input$third == ''){char3 = '03703'} else {char3 = input$third}
  #   if(input$fourth == ''){char4 = '03704'} else {char4 = input$fourth}
  # 
  #   data.frame(
  #     N01 = filter(Incomes, PUMA == char1) %>% select(HINCP),
  #     N02 = filter(Incomes, PUMA == char2) %>% select(HINCP),
  #     N03 = filter(Incomes, PUMA == char3) %>% select(HINCP),
  #     N04 = filter(Incomes, PUMA == char4) %>% select(HINCP)
  #   )
  #   })
  output$mylines = renderPlot({
    PlotVals = filter(Incomes, PUMA == '04011')
    g = ggplot(data = PlotVals, aes(x = Year, y = HINCP)) + geom_line()
    g
  })
  
  output$heat = renderPlot({
    shades = colorRampPalette(c('orange', 'blue'))(100)
    Selection = filter(ACS_P, Year == input$timeframe) %>% select(PUMA, match(input$mapvar, names(ACS_P)))
    filltab = shades[Selection$RACE_W]
    filltab = cbind(Selection, fills2 = filltab)
    
    nyc = get_map(
      location = 'newyork',
      zoom = 6,
      color = 'bw',
      maptype = 'terrain')
    
    shapes.points = fortify(shapes)
    
    #id = match(filltab$PUMA, shapes$PUMACE10) - 1
    id = 1:length(filltab$PUMA)
    
    filltab = cbind(filltab, id)
    
    fillcolors = merge(shapes.points, filltab, by = 'id', all.x = TRUE)$fills2
    
    # Colors is what will changed on values.
    #Length is 142843
    
    
    polymap = ggmap(nyc) +
      geom_polygon(
        aes(x = long,
            y = lat,
            group = group
        ),
        data = shapes.points,
        color = 'dark Violet',
        fill = fillcolors,
        alpha = 0.5
      ) +
      coord_map(xlim = c(-80, -71.5), ylim = c(40,45.5)) +
      ggtitle(sprintf('New York Geographical Gradient of %s over PUMAs', input$mapvar))
    
    polymap
  })
})
