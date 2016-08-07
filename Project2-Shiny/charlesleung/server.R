#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#Shiny Project


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
  #   char1 = '03701'
  #   char2 = '03702'
  #   char3 = '03703'
  #   char4 = '03704'
  #   
  #   data.frame(
  #     N01 = filter(Incomes, PUMA == char1) %>% select(HINCP),
  #     N02 = filter(Incomes, PUMA == char2) %>% select(HINCP),
  #     N03 = filter(Incomes, PUMA == char3) %>% select(HINCP),
  #     N04 = filter(Incomes, PUMA == char4) %>% select(HINCP)
  #   )
  #   })
  
  test1 = filter(Incomes, PUMA == '03805')[[3]]
  PUMA_03802 = c(29850, 32000, 35000, 40050, 41000, 40000, 41450, 36900, 47600, 50000)
  PUMA_03805 = c(85000, 96875, 95000, 104200, 97550, 95000, 124500, 112850, 118150, 124200)
  PUMA_04008 = c(43320, 46400, 45800, 48100, 46500, 43000, 44800, 42500, 46200, 46500)
  
  linedf <- data.frame(
    
    PUMA_03802,
    PUMA_03805,
    PUMA_04008
  )
  
  # test1 = as.data.frame(filter(Incomes, PUMA == '04011')$HINCP)
  
  output$mychart <- renderLineChart({
    # Return a data frame. Each column will be a series in the line chart.
  linedf
  })
  
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
